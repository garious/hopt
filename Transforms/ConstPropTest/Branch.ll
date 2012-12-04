define i32 @test1(i1 %B) {
  br i1 %B, label %BB1, label %BB2

BB1:      
  %Val = add i32 0, 0
  br label %BB3

BB2:      
  br label %BB3

BB3:     
  %Ret = phi i32 [ %Val, %BB1 ], [ 0, %BB2 ] 
  ret i32 %Ret
}

