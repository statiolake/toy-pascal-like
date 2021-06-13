begin
    function Fib(n: int): int; begin
        if n == 0 then
            Fib := 0
        else if n == 1 then
            Fib := 1
        else begin
            Fib := (Fib((n - 1)) + Fib((n - 2)))
        end
    end;

    x := Fib(25);
    dump x
end
