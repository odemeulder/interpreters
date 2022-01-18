  program Main;
    var a, b : integer;
  begin
    a := 9;
    { Single line statement }
    if a > 1 then
      writeln('a is greater than b'); {ends here with the semi colon}
    
    { Multiple statements by using the compound_statement remenber? BEGIN->END}
    if a < 10 then
    begin
      WriteLn('Why a is less than 10?');
      WriteLn('I told you that a will never be less than 10');
      WriteLn('So fix it already... damn it!');
      a := a + 1;
      WriteLn('Much better :-)') {the semi colon in the last sentence is optional :-)}
    end;

    { Else Statement }
    if a > 10 then
      WriteLn('Nice, I like everything grater than 10')
    else
      WriteLn('Fix it again and dont make me lose my money!'); {this unique semi colon finishes the whole sentence}
    
  end.