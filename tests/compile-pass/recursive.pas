begin
    function Fib(n: int): int; begin
        if n = 0 then
            Fib := 0
        else if n = 1 then
            Fib := 1
        else
            Fib := (Fib((n - 1)) + Fib((n - 2)))
    end;

    x := Fib(10);
    dump x
end
