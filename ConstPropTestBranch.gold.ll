; ModuleID = 'ConstPropTestBranch.ll'

define i32 @test1(B) {
  br %B label %BB1, label %BB2

BB1:
  br %BB3

BB2:
  br %BB3

BB3:
  ret i32 0
}
