define i32 @a() {
  %1 = add i32 1, 1
  %2 = %1
:flush        ; Flush nothing
  %3 = %2
:flush        ; Flush nothing
  ret i32 %3
}

