-module(simple).
-export([foo/1]).

foo(_) -> ok.


bar(Apple, Pear) -> london, foo(Pear).


%  [[-,module,'(',simple,')','.'],[-,export,'(','[',foo,/,1,']',')','.'],[],[foo,'(','_',')',->,ok,'.'],[],[],
% [bar,'(','Apple',',','Pear',')',->,london,',',foo,'(','Pear',')','.']]