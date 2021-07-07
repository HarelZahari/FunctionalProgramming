% Harel Zahari
% 305494452
% Naor Zaharia
% 312423841

-module(hw6).
-export([rec_fn/1]).
-export([zip/2]).
-export([foldl/3]).
-export([scalar_product/2]).
-export([mat_mul/2]).
-export([mat_mul_service/3]).

% Ex 1
rec_fn_tailRec(1, A, _) -> A;
rec_fn_tailRec(N, A, B) -> rec_fn_tailRec(N - 1, B, 2 * B - 3 * A).

rec_fn(1) -> 1;
rec_fn(2) -> 1;
rec_fn(N) -> rec_fn_tailRec(N, 1, 1).

% Ex2
zip ([],_) -> [];
zip (_,[]) -> [];
zip([HdA | TlA], [HdB | TlB]) -> [createTuple(HdA,HdB)] ++ zip(TlA,TlB).

createTuple(X,Y) -> {X,Y}.

% Ex3
foldl(_,A,[]) -> A;
foldl(F,A,[Hd | Tl]) -> foldl(F ,(F(A, Hd)), Tl).

% Ex4
scalar_product([],[]) -> 0;
scalar_product ([HdA | TlA], [HdB | TlB]) -> (HdA * HdB) + scalar_product(TlA,TlB).

% Ex5
mat_mul([[A11,A12], [A21,A22]], [[B11,B12], [B21,B22]]) ->
PIDM22 = spawn(hw6, mat_mul_service,[self(), [A21,A22], [B12,B22]]),
PIDM21 = spawn(hw6, mat_mul_service, [PIDM22, [A21,A22], [B11,B21]]),
PIDM12 = spawn(hw6, mat_mul_service, [PIDM21, [A11,A12], [B12,B22]]),
PIDM11 = spawn(hw6, mat_mul_service, [PIDM12,[A21,A22], [B11,B21]]),
PIDM11 ! {m11},
receive
    {Matrix} -> MatrixResult = Matrix
end,
MatrixResult.

mat_mul_service(NextPID, R, C) ->
receive
{m11} -> NextPID ! {m12, scalar_product(R,C)};
{m12, M11} -> NextPID ! {m21, M11, scalar_product(R,C)};
{m21, M11, M12} -> NextPID ! {m22, M11, M12, scalar_product(R,C)};
{m22, M11, M12, M21} -> NextPID ! {[[M11, M12],[M21, scalar_product(R,C)]]}
end.