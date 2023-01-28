-module(arithmetics).
-export([foo/1, main/0]).   

foo(_Alma) ->
    5+4, % expression
    683*2.

main() ->
    foo(whatever).


% [[-,module,'(',arithmetics,')','.'],[-,export,'(','[',foo,/,1,',',main,/,0,']',')','.'],[],[foo,'(','_Alma',')',->],[5,+,4,',','%',expression],[683,*,2,'.'],[],[main,'(',')',->],[foo,'(',whatever,')','.'],[],[]]
% 