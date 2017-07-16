private
function GetGG:TVector2; {$ifdef CAN_INLINE}inline;{$endif}
function GetGR:TVector2; {$ifdef CAN_INLINE}inline;{$endif}
function GetRG:TVector2; {$ifdef CAN_INLINE}inline;{$endif}
function GetRR:TVector2; {$ifdef CAN_INLINE}inline;{$endif}
function GetSS:TVector2; {$ifdef CAN_INLINE}inline;{$endif}
function GetST:TVector2; {$ifdef CAN_INLINE}inline;{$endif}
function GetTS:TVector2; {$ifdef CAN_INLINE}inline;{$endif}
function GetTT:TVector2; {$ifdef CAN_INLINE}inline;{$endif}
function GetXX:TVector2; {$ifdef CAN_INLINE}inline;{$endif}
function GetXY:TVector2; {$ifdef CAN_INLINE}inline;{$endif}
function GetYX:TVector2; {$ifdef CAN_INLINE}inline;{$endif}
function GetYY:TVector2; {$ifdef CAN_INLINE}inline;{$endif}
procedure SetGR(const pValue:TVector2); {$ifdef CAN_INLINE}inline;{$endif}
procedure SetRG(const pValue:TVector2); {$ifdef CAN_INLINE}inline;{$endif}
procedure SetST(const pValue:TVector2); {$ifdef CAN_INLINE}inline;{$endif}
procedure SetTS(const pValue:TVector2); {$ifdef CAN_INLINE}inline;{$endif}
procedure SetXY(const pValue:TVector2); {$ifdef CAN_INLINE}inline;{$endif}
procedure SetYX(const pValue:TVector2); {$ifdef CAN_INLINE}inline;{$endif}
public
property gg:TVector2 read GetGG;
property gr:TVector2 read GetGR write SetGR;
property rg:TVector2 read GetRG write SetRG;
property rr:TVector2 read GetRR;
property ss:TVector2 read GetSS;
property st:TVector2 read GetST write SetST;
property ts:TVector2 read GetTS write SetTS;
property tt:TVector2 read GetTT;
property xx:TVector2 read GetXX;
property xy:TVector2 read GetXY write SetXY;
property yx:TVector2 read GetYX write SetYX;
property yy:TVector2 read GetYY;
