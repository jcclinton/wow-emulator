% network defines
-define(IL, /unsigned-little-integer).
-define(IB, /unsigned-big-integer).
-define(f, :32/float-little).

-define(K, :1024?IL).
-define(KB, :1024?IB).
-define(KH, :512?IL).
-define(KHB, :512?IB).

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
-define(G, :24?IL). % used for guid
-define(W, :16?IL). % word
-define(WO, :16?IB). % word, big endian
-define(B, :8). %byte
