/*
   Copyright (C) 2001-2007 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

/*

  Copyright (C) 2000 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement
  or the like.  Any license provided herein, whether implied or
  otherwise, applies only to this software file.  Patent licenses, if
  any, provided herein do not apply to combinations of this program with
  other software, or any other product whatsoever.

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/

#ifndef cflow_common_INCLUDED
#define cflow_common_INCLUDED

/* Tracing:
 */
#define TRACE_CFLOW     0x0001
#define TRACE_DETAIL    0x0002
#define TRACE_UNREACH   0x0004
#define TRACE_BRANCH    0x0008
#define TRACE_MERGE     0x0010
#define TRACE_REORDER   0x0020
#define TRACE_FREQ_ORDER 0x0040
#define TRACE_CLONE     0x0080
#define TRACE_FREQ      0x0100  /* used in freq.c */
#define TRACE_DOM       0x0200  /* used in dominate.c */
#define TRACE_LAYOUT    0x0400 


extern BOOL CFLOW_Trace;
extern BOOL CFLOW_Trace_Detail;
extern BOOL CFLOW_Trace_Unreach;
extern BOOL CFLOW_Trace_Branch;
extern BOOL CFLOW_Trace_Merge;
extern BOOL CFLOW_Trace_Reorder;
extern BOOL CFLOW_Trace_Freq_Order;
extern BOOL CFLOW_Trace_Clone;
extern BOOL CFLOW_Trace_Freq;
extern BOOL CFLOW_Trace_Dom;
extern BOOL CFLOW_Trace_Layout;


/* We need to keep some auxilary information for each BB for the various
 * optimizations we perform. The following BB_MAP provides the mechanism
 * to access the info.
 */
extern BB_MAP bb_info_map;

/* This is the auxiliary info we keep for each BB.
 */
struct logif_info {
  TN *tn1;			/* branch condition tn 1 */
  TN *tn2;			/* branch condition tn 2 */
  OP *compare_op;		/* the OP that compares tn 1 and tn 2 */
  mUINT8 variant;		/* branch variant */
  mBOOL b_likely;		/* branch was a branch-likely */
};

struct vargoto_info {
  ST *listvar;			/* label table symbol */
  INT *refcount;		/* jump table reference count */
};

struct succedge {
  INT64 offset;			/* offset (in bytes) in succ */
  BB *bb;			/* the successor BB */
  float prob;			/* probability this edge is taken */
};

#define BBINFO_NSUCCS (2)	/* Number of successor edges that are
				 * are guaranteed to be allocated
				 * for a BBINFO struct. This number
				 * is large enough for all kinds
				 * except VARGOTO and INDGOTO
				 */

typedef struct bbinfo {
  BBKIND kind;			/* BB kind */
  union {			/* kind specific info */
    struct logif_info l;
    struct vargoto_info v;
  } u;
  mUINT16 eh_rgn;		/* exc handling region number */
  mBOOL cold;			/* part of cold region */
  INT nsuccs;			/* number of successors */
  struct succedge succs[BBINFO_NSUCCS]; /* successor edges; we dynamically
				 * allocate this so it MUST BE LAST.
				 */
} BBINFO;

/* BB-info accessors:
 */
#define BB_BBINFO(bb) ((BBINFO *)BB_MAP_Get(bb_info_map, (bb)))

#define     BBINFO_kind(b)		((BBKIND)BB_BBINFO(b)->kind)
#define Set_BBINFO_kind(b,k)		(BB_BBINFO(b)->kind=(k))
#define     BBINFO_eh_rgn(b)		(BB_BBINFO(b)->eh_rgn+0)
#define Set_BBINFO_eh_rgn(b, e)		(BB_BBINFO(b)->eh_rgn=(e))
#define     BBINFO_cold(b)		(BB_BBINFO(b)->cold+0)
#define Set_BBINFO_cold(b, e)		(BB_BBINFO(b)->cold=(e))
#define     BBINFO_nsuccs(b)		(BB_BBINFO(b)->nsuccs+0)
#define Set_BBINFO_nsuccs(b,n)		(BB_BBINFO(b)->nsuccs=(n))
#define     BBINFO_succ_bb(b, n)	(BB_BBINFO(b)->succs[n].bb+0)
#define Set_BBINFO_succ_bb(b, n, s)	(BB_BBINFO(b)->succs[n].bb=(s))
#define     BBINFO_succ_offset(b, n)	(BB_BBINFO(b)->succs[n].offset+0)
#define Set_BBINFO_succ_offset(b, n, o)	(BB_BBINFO(b)->succs[n].offset=(o))
#define     BBINFO_succ_prob(b, n)	(BB_BBINFO(b)->succs[n].prob+0)
#define Set_BBINFO_succ_prob(b, n, p)	(BB_BBINFO(b)->succs[n].prob=(p))
#define     BBINFO_variant(b)		(BB_BBINFO(b)->u.l.variant+0)
#define Set_BBINFO_variant(b, v)	(BB_BBINFO(b)->u.l.variant=(v))
#define     BBINFO_b_likely(b)		(BB_BBINFO(b)->u.l.b_likely+0)
#define Set_BBINFO_b_likely(b, v)	(BB_BBINFO(b)->u.l.b_likely=(v))
#define     BBINFO_condval1(b)		(BB_BBINFO(b)->u.l.tn1+0)
#define Set_BBINFO_condval1(b, tn)	(BB_BBINFO(b)->u.l.tn1=(tn))
#define     BBINFO_condval2(b)		(BB_BBINFO(b)->u.l.tn2+0)
#define Set_BBINFO_condval2(b, tn)	(BB_BBINFO(b)->u.l.tn2=(tn))
#define     BBINFO_compare_op(b)	(BB_BBINFO(b)->u.l.compare_op+0)
#define Set_BBINFO_compare_op(b, op)	(BB_BBINFO(b)->u.l.compare_op=(op))
#define     BBINFO_vargoto_listvar(b)	(BB_BBINFO(b)->u.v.listvar+0)
#define Set_BBINFO_vargoto_listvar(b, l) (BB_BBINFO(b)->u.v.listvar=(l))
#define     BBINFO_vargoto_refcount(b)	(BB_BBINFO(b)->u.v.refcount+0)
#define Set_BBINFO_vargoto_refcount(b, r) (BB_BBINFO(b)->u.v.refcount=(r))

/* The following data structure holds information about successor
 * edges in the CFG.
 */
typedef struct edge {
  INT32 id;             /* edge index */
  BB *pred;             /* predecessor node */
  BB *succ;             /* successor node */
  struct edge *preds;   /* list of predecessor edges of 'succ' */
  double freq;          /* frequency this edge is taken */
  double weight;        /* weight assigned to this edge */
} EDGE;



extern INT Count_Succ_Edges(void);
extern INT Init_Edges(EDGE *edges);
extern BOOL Freq_BB_LAYOUT(void);


#endif
