begin
    f := 0;
    s := 1;
    n := 0;
    while n < 5 do begin
        t := (f + s);
        f := s;
        s := t;
        n := (n + 1)
    end
end
