; ModuleID = 'ConstPropTest/Branch.ll'

define i32 @test1(i1 %B) {
  br i1 %B, label %BB1, label %BB2

BB1:
  br label %BB3

BB2:
  br label %BB3

BB3:
  ret i32 0
}
