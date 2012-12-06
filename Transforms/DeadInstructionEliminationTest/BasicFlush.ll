define i32 @a() {
  %1 = add i32 1, 1
  %2 = add i32 %1, 1
:flush                   ; %1 and %2 should be flushed
  %3 = add i32 1, %2
:flush                   ; %3 needs to be flushed.  We don't know that it won't be used.
  ret i32 %2
}

