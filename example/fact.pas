begin
    ans := Factorial(5);
    dump ans;

    function Factorial(n: int): int; begin
        if n = 0 then
            Factorial := 1
        else
            Factorial := n * Factorial(n - 1)
    end
end
