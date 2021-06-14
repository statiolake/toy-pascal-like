begin
    function IsEven(x: int): bool; begin
        IsEven := x / 2 * 2 = x
    end;
    x := IsEven(2);
    y := IsEven(3);
    z := IsEven(10)
end
