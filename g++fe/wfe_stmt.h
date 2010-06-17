
/* 
   Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

/* WFE == WHIRL Front End */
/* translate gnu stmt trees to whirl */

#ifndef wn_stmt_INCLUDED
#define wn_stmt_INCLUDED

extern void WFE_Stmt_Init (void);
extern LABEL_IDX WFE_Get_LABEL (tree label, int def);
extern void WFE_Check_Undefined_Labels(void);
extern void Mark_Scopes_And_Labels (tree);
extern void Push_Temp_Cleanup (tree, bool, bool);
extern void Do_Temp_Cleanups (tree);
extern void WFE_Expand_Handlers_And_Cleanups (void);
extern void Call_Throw();
extern INT Current_Handler_Count (void);
extern void Add_Handler_Info (WN * call_wn, INT i, INT num_handlers);
extern WN *WFE_Start_Try_Block (void);
extern void WFE_Finish_Try_Block (WN *region, tree handler);
extern void Push_Conditional_Cleanup_Level (WN *uncond_region =NULL, WN *uncond_after =NULL);
extern void Pop_Conditional_Cleanup_Level (void);

extern
#ifdef __cplusplus
"C"
#endif
void WFE_Expand_Stmt (tree stmt);

#endif
