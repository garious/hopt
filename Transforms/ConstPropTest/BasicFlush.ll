define i32 @a() {
  %1 = add i32 1, 1
  %2 = add i32 %1, 1
:flush                 ; Flush %1 and %2.  Ensure %1 is flushed before %2.
  %3 = add i32 1, %2
:flush                 ; Flush %3.  Ensure %1 and %2 are flushed a second time.
  ret i32 %3
}

