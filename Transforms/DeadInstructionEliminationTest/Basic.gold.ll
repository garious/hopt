; ModuleID = 'DeadInstructionEliminationTest/Basic.ll'

define i32 @a() {
  %1 = add i32 1, 1
  %2 = add i32 %1, 1
  ret i32 %2
}
