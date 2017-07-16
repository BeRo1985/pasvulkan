function TVector2.GetXX:TVector2;
begin
 result.x:=x;
 result.y:=x;
end;
function TVector2.GetXY:TVector2;
begin
 result.x:=x;
 result.y:=y;
end;
procedure TVector2.SetXY(const pValue:TVector2);
begin
 x:=pValue.x;
 y:=pValue.y;
end;
function TVector2.GetYX:TVector2;
begin
 result.x:=y;
 result.y:=x;
end;
procedure TVector2.SetYX(const pValue:TVector2);
begin
 y:=pValue.x;
 x:=pValue.y;
end;
function TVector2.GetYY:TVector2;
begin
 result.x:=y;
 result.y:=y;
end;
function TVector2.GetRR:TVector2;
begin
 result.r:=r;
 result.g:=r;
end;
function TVector2.GetRG:TVector2;
begin
 result.r:=r;
 result.g:=g;
end;
procedure TVector2.SetRG(const pValue:TVector2);
begin
 r:=pValue.r;
 g:=pValue.g;
end;
function TVector2.GetGR:TVector2;
begin
 result.r:=g;
 result.g:=r;
end;
procedure TVector2.SetGR(const pValue:TVector2);
begin
 g:=pValue.r;
 r:=pValue.g;
end;
function TVector2.GetGG:TVector2;
begin
 result.r:=g;
 result.g:=g;
end;
function TVector2.GetSS:TVector2;
begin
 result.s:=s;
 result.t:=s;
end;
function TVector2.GetST:TVector2;
begin
 result.s:=s;
 result.t:=t;
end;
procedure TVector2.SetST(const pValue:TVector2);
begin
 s:=pValue.s;
 t:=pValue.t;
end;
function TVector2.GetTS:TVector2;
begin
 result.s:=t;
 result.t:=s;
end;
procedure TVector2.SetTS(const pValue:TVector2);
begin
 t:=pValue.s;
 s:=pValue.t;
end;
function TVector2.GetTT:TVector2;
begin
 result.s:=t;
 result.t:=t;
end;
