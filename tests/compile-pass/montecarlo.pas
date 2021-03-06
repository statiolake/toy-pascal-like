begin
    function Pi(): float; begin
        i := 0;
        inside := 0;
        while i < 100000 do begin
            x := RandomFloat();
            y := RandomFloat();
            if x * x + y * y <= 1.0
                then inside := inside + 1
                else begin end;
            i := i + 1
        end;
        Pi := 4.0 * inttofloat(inside) / inttofloat(i)
    end;

    pi := Pi();
    dump pi
end
