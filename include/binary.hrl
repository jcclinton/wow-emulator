% network defines
-define(SIL, /signed-little-integer).
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

-define(SD, :320?IL). % sha1 double
-define(SDB, :320?IB). % sha1 double big endian
-define(SH, :160?IL). % sha1
-define(SHB, :160?IB). % sha1 big endian

-define(Q, :64?IL). % quad
-define(QB, :64?IB). % quad
-define(L, :32?IL). % long
-define(SL, :32?SIL). % signed long
-define(LB, :32?IB). % long
-define(G, :24?IL). % used for guid
-define(W, :16?IL). % word
-define(WO, :16?IB). % word, big endian
-define(B, :8). %byte
