-module(funeval).
-export([main/0]).   

foo(Alma) -> Alma.

main() -> 
    foo(5*4).
