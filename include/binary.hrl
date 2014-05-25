% network defines
-define(I, /unsigned-integer).
-define(IL, /unsigned-little-integer).
-define(IB, /unsigned-big-integer).
-define(b, /bytes).
-define(f, :32/float-little).

-define(QQ, :256?IL).
-define(QQB, :256?IB).
-define(QH, :128?IL).

-define(SL, :320?IL). % sha1
-define(SLB, :320?IB). % sha1
-define(SH, :160?IL). % sha1
-define(SHB, :160?IB). % sha1

-define(Q, :64?IL). % quad
-define(L, :32?IL). % long
-define(LB, :32?IB). % long
-define(W, :16?IL). % word
-define(WO, :16?IB). % word, big endian
-define(B, :8). %byte
