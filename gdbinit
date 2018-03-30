set history save
set print pretty on
set prompt \001\033[01;31m\002gdb $ \001\033[0m\002

define dtailq
 set $next = $arg0.tqh_first
 while ($next != 0)
  p $next
  p *$next
  set $next = $next.$arg1.tqe_next
 end
end

define dslist
 set $next = $arg0.slh_first
 while ($next != 0)
  p $next
  set $next = $next.$arg1.sle_next
 end
end
