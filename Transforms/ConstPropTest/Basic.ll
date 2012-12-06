define i32 @a() {
  %1 = add i32 1, 1
  %2 = add i32 %1, 1
  %3 = add i32 1, %2
  ret i32 %3
}

