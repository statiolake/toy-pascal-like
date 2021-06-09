begin
    a_0 := 0;
    a_1 := 1;
    i := 0;
    while i < 20 do begin
        dump a_0;
        tmp := a_0 + a_1;
        a_0 := a_1;
        a_1 := tmp;
        i := i + 1
    end
end
