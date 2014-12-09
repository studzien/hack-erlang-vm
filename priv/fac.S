{module, fac}.  %% version = 0

{exports, [{fac,1},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 10}.


{function, fac, 1, 2}.
  {label,1}.
    {line,[{location,"fac.erl",6}]}.
    {func_info,{atom,fac},{atom,fac},1}.
  {label,2}.
    {move,{literal,{state,1}},{x,1}}.
    {call_only,2,{f,4}}.


{function, fac, 2, 4}.
  {label,3}.
    {line,[{location,"fac.erl",9}]}.
    {func_info,{atom,fac},{atom,fac},2}.
  {label,4}.
    {test,is_eq_exact,{f,5},[{x,0},{integer,0}]}.
    {test,is_tuple,{f,3},[{x,1}]}.
    {test,test_arity,{f,5},[{x,1},2]}.
    {get_tuple_element,{x,1},0,{x,2}}.
    {get_tuple_element,{x,1},1,{x,3}}.
    {test,is_eq_exact,{f,5},[{x,2},{atom,state}]}.
    {move,{x,3},{x,0}}.
    return.
  {label,5}.
    {test,is_tuple,{f,3},[{x,1}]}.
    {test,test_arity,{f,3},[{x,1},2]}.
    {get_tuple_element,{x,1},0,{x,2}}.
    {get_tuple_element,{x,1},1,{x,3}}.
    {test,is_eq_exact,{f,3},[{x,2},{atom,state}]}.
    {line,[{location,"fac.erl",12}]}.
    {gc_bif,'-',{f,0},4,[{x,0},{integer,1}],{x,1}}.
    {line,[{location,"fac.erl",12}]}.
    {gc_bif,'*',{f,0},4,[{x,3},{x,0}],{x,0}}.
    {test_heap,3,4}.
    {put_tuple,2,{x,2}}.
    {put,{atom,state}}.
    {put,{x,0}}.
    {move,{x,1},{x,0}}.
    {move,{x,2},{x,1}}.
    {call_only,2,{f,4}}.


{function, module_info, 0, 7}.
  {label,6}.
    {line,[]}.
    {func_info,{atom,fac},{atom,module_info},0}.
  {label,7}.
    {move,{atom,fac},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 9}.
  {label,8}.
    {line,[]}.
    {func_info,{atom,fac},{atom,module_info},1}.
  {label,9}.
    {move,{x,0},{x,1}}.
    {move,{atom,fac},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
