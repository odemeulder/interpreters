program fibonaci;
var
   n, ret : integer;

function fib(n: integer): integer;
var
   { local variable declaration }
   result: integer;

begin
   if (n = 0) then
      result := 0
   else if (n = 1) then
      result := 1
   else
      result := fib(n - 2) + fib(n - 1);

   fib := result;
end;

begin

   ret := fib(9);
   
   writeln( 'Fibonacci sequence: 1 1 2 3 5 8 13 21 34 55');
   write( 'Fib 9 : ');
   writeln(ret);
end.