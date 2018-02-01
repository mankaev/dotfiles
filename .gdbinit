add-auto-load-safe-path .
set print pretty on
set height 0
set print sevenbit-strings off
set print asm-demangle on
set print object on
set print symbol-filename on

define mall
  call malloc_stats()
end

document mall
   my redefinition of malloc_stats() function call
end

