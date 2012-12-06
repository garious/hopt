; ModuleID = 'ConstPropTest/BasicFlush.ll'

define i32 @a() {
  %1 = 2
  %2 = 3
  %3 = 4
  ret i32 4
}
