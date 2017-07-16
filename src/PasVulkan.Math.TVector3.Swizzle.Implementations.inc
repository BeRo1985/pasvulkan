function TVector3.GetXX:TVector2;
begin
 result.x:=x;
 result.y:=x;
end;
function TVector3.GetXXX:TVector3;
begin
 result.x:=x;
 result.y:=x;
 result.z:=x;
end;
function TVector3.GetXXY:TVector3;
begin
 result.x:=x;
 result.y:=x;
 result.z:=y;
end;
function TVector3.GetXXZ:TVector3;
begin
 result.x:=x;
 result.y:=x;
 result.z:=z;
end;
function TVector3.GetXY:TVector2;
begin
 result.x:=x;
 result.y:=y;
end;
procedure TVector3.SetXY(const pValue:TVector2);
begin
 x:=pValue.x;
 y:=pValue.y;
end;
function TVector3.GetXYX:TVector3;
begin
 result.x:=x;
 result.y:=y;
 result.z:=x;
end;
function TVector3.GetXYY:TVector3;
begin
 result.x:=x;
 result.y:=y;
 result.z:=y;
end;
function TVector3.GetXYZ:TVector3;
begin
 result.x:=x;
 result.y:=y;
 result.z:=z;
end;
procedure TVector3.SetXYZ(const pValue:TVector3);
begin
 x:=pValue.x;
 y:=pValue.y;
 z:=pValue.z;
end;
function TVector3.GetXZ:TVector2;
begin
 result.x:=x;
 result.y:=z;
end;
procedure TVector3.SetXZ(const pValue:TVector2);
begin
 x:=pValue.x;
 z:=pValue.y;
end;
function TVector3.GetXZX:TVector3;
begin
 result.x:=x;
 result.y:=z;
 result.z:=x;
end;
function TVector3.GetXZY:TVector3;
begin
 result.x:=x;
 result.y:=z;
 result.z:=y;
end;
procedure TVector3.SetXZY(const pValue:TVector3);
begin
 x:=pValue.x;
 z:=pValue.y;
 y:=pValue.z;
end;
function TVector3.GetXZZ:TVector3;
begin
 result.x:=x;
 result.y:=z;
 result.z:=z;
end;
function TVector3.GetYX:TVector2;
begin
 result.x:=y;
 result.y:=x;
end;
procedure TVector3.SetYX(const pValue:TVector2);
begin
 y:=pValue.x;
 x:=pValue.y;
end;
function TVector3.GetYXX:TVector3;
begin
 result.x:=y;
 result.y:=x;
 result.z:=x;
end;
function TVector3.GetYXY:TVector3;
begin
 result.x:=y;
 result.y:=x;
 result.z:=y;
end;
function TVector3.GetYXZ:TVector3;
begin
 result.x:=y;
 result.y:=x;
 result.z:=z;
end;
procedure TVector3.SetYXZ(const pValue:TVector3);
begin
 y:=pValue.x;
 x:=pValue.y;
 z:=pValue.z;
end;
function TVector3.GetYY:TVector2;
begin
 result.x:=y;
 result.y:=y;
end;
function TVector3.GetYYX:TVector3;
begin
 result.x:=y;
 result.y:=y;
 result.z:=x;
end;
function TVector3.GetYYY:TVector3;
begin
 result.x:=y;
 result.y:=y;
 result.z:=y;
end;
function TVector3.GetYYZ:TVector3;
begin
 result.x:=y;
 result.y:=y;
 result.z:=z;
end;
function TVector3.GetYZ:TVector2;
begin
 result.x:=y;
 result.y:=z;
end;
procedure TVector3.SetYZ(const pValue:TVector2);
begin
 y:=pValue.x;
 z:=pValue.y;
end;
function TVector3.GetYZX:TVector3;
begin
 result.x:=y;
 result.y:=z;
 result.z:=x;
end;
procedure TVector3.SetYZX(const pValue:TVector3);
begin
 y:=pValue.x;
 z:=pValue.y;
 x:=pValue.z;
end;
function TVector3.GetYZY:TVector3;
begin
 result.x:=y;
 result.y:=z;
 result.z:=y;
end;
function TVector3.GetYZZ:TVector3;
begin
 result.x:=y;
 result.y:=z;
 result.z:=z;
end;
function TVector3.GetZX:TVector2;
begin
 result.x:=z;
 result.y:=x;
end;
procedure TVector3.SetZX(const pValue:TVector2);
begin
 z:=pValue.x;
 x:=pValue.y;
end;
function TVector3.GetZXX:TVector3;
begin
 result.x:=z;
 result.y:=x;
 result.z:=x;
end;
function TVector3.GetZXY:TVector3;
begin
 result.x:=z;
 result.y:=x;
 result.z:=y;
end;
procedure TVector3.SetZXY(const pValue:TVector3);
begin
 z:=pValue.x;
 x:=pValue.y;
 y:=pValue.z;
end;
function TVector3.GetZXZ:TVector3;
begin
 result.x:=z;
 result.y:=x;
 result.z:=z;
end;
function TVector3.GetZY:TVector2;
begin
 result.x:=z;
 result.y:=y;
end;
procedure TVector3.SetZY(const pValue:TVector2);
begin
 z:=pValue.x;
 y:=pValue.y;
end;
function TVector3.GetZYX:TVector3;
begin
 result.x:=z;
 result.y:=y;
 result.z:=x;
end;
procedure TVector3.SetZYX(const pValue:TVector3);
begin
 z:=pValue.x;
 y:=pValue.y;
 x:=pValue.z;
end;
function TVector3.GetZYY:TVector3;
begin
 result.x:=z;
 result.y:=y;
 result.z:=y;
end;
function TVector3.GetZYZ:TVector3;
begin
 result.x:=z;
 result.y:=y;
 result.z:=z;
end;
function TVector3.GetZZ:TVector2;
begin
 result.x:=z;
 result.y:=z;
end;
function TVector3.GetZZX:TVector3;
begin
 result.x:=z;
 result.y:=z;
 result.z:=x;
end;
function TVector3.GetZZY:TVector3;
begin
 result.x:=z;
 result.y:=z;
 result.z:=y;
end;
function TVector3.GetZZZ:TVector3;
begin
 result.x:=z;
 result.y:=z;
 result.z:=z;
end;
function TVector3.GetRR:TVector2;
begin
 result.r:=r;
 result.g:=r;
end;
function TVector3.GetRRR:TVector3;
begin
 result.r:=r;
 result.g:=r;
 result.b:=r;
end;
function TVector3.GetRRG:TVector3;
begin
 result.r:=r;
 result.g:=r;
 result.b:=g;
end;
function TVector3.GetRRB:TVector3;
begin
 result.r:=r;
 result.g:=r;
 result.b:=b;
end;
function TVector3.GetRG:TVector2;
begin
 result.r:=r;
 result.g:=g;
end;
procedure TVector3.SetRG(const pValue:TVector2);
begin
 r:=pValue.r;
 g:=pValue.g;
end;
function TVector3.GetRGR:TVector3;
begin
 result.r:=r;
 result.g:=g;
 result.b:=r;
end;
function TVector3.GetRGG:TVector3;
begin
 result.r:=r;
 result.g:=g;
 result.b:=g;
end;
function TVector3.GetRGB:TVector3;
begin
 result.r:=r;
 result.g:=g;
 result.b:=b;
end;
procedure TVector3.SetRGB(const pValue:TVector3);
begin
 r:=pValue.r;
 g:=pValue.g;
 b:=pValue.b;
end;
function TVector3.GetRB:TVector2;
begin
 result.r:=r;
 result.g:=b;
end;
procedure TVector3.SetRB(const pValue:TVector2);
begin
 r:=pValue.r;
 b:=pValue.g;
end;
function TVector3.GetRBR:TVector3;
begin
 result.r:=r;
 result.g:=b;
 result.b:=r;
end;
function TVector3.GetRBG:TVector3;
begin
 result.r:=r;
 result.g:=b;
 result.b:=g;
end;
procedure TVector3.SetRBG(const pValue:TVector3);
begin
 r:=pValue.r;
 b:=pValue.g;
 g:=pValue.b;
end;
function TVector3.GetRBB:TVector3;
begin
 result.r:=r;
 result.g:=b;
 result.b:=b;
end;
function TVector3.GetGR:TVector2;
begin
 result.r:=g;
 result.g:=r;
end;
procedure TVector3.SetGR(const pValue:TVector2);
begin
 g:=pValue.r;
 r:=pValue.g;
end;
function TVector3.GetGRR:TVector3;
begin
 result.r:=g;
 result.g:=r;
 result.b:=r;
end;
function TVector3.GetGRG:TVector3;
begin
 result.r:=g;
 result.g:=r;
 result.b:=g;
end;
function TVector3.GetGRB:TVector3;
begin
 result.r:=g;
 result.g:=r;
 result.b:=b;
end;
procedure TVector3.SetGRB(const pValue:TVector3);
begin
 g:=pValue.r;
 r:=pValue.g;
 b:=pValue.b;
end;
function TVector3.GetGG:TVector2;
begin
 result.r:=g;
 result.g:=g;
end;
function TVector3.GetGGR:TVector3;
begin
 result.r:=g;
 result.g:=g;
 result.b:=r;
end;
function TVector3.GetGGG:TVector3;
begin
 result.r:=g;
 result.g:=g;
 result.b:=g;
end;
function TVector3.GetGGB:TVector3;
begin
 result.r:=g;
 result.g:=g;
 result.b:=b;
end;
function TVector3.GetGB:TVector2;
begin
 result.r:=g;
 result.g:=b;
end;
procedure TVector3.SetGB(const pValue:TVector2);
begin
 g:=pValue.r;
 b:=pValue.g;
end;
function TVector3.GetGBR:TVector3;
begin
 result.r:=g;
 result.g:=b;
 result.b:=r;
end;
procedure TVector3.SetGBR(const pValue:TVector3);
begin
 g:=pValue.r;
 b:=pValue.g;
 r:=pValue.b;
end;
function TVector3.GetGBG:TVector3;
begin
 result.r:=g;
 result.g:=b;
 result.b:=g;
end;
function TVector3.GetGBB:TVector3;
begin
 result.r:=g;
 result.g:=b;
 result.b:=b;
end;
function TVector3.GetBR:TVector2;
begin
 result.r:=b;
 result.g:=r;
end;
procedure TVector3.SetBR(const pValue:TVector2);
begin
 b:=pValue.r;
 r:=pValue.g;
end;
function TVector3.GetBRR:TVector3;
begin
 result.r:=b;
 result.g:=r;
 result.b:=r;
end;
function TVector3.GetBRG:TVector3;
begin
 result.r:=b;
 result.g:=r;
 result.b:=g;
end;
procedure TVector3.SetBRG(const pValue:TVector3);
begin
 b:=pValue.r;
 r:=pValue.g;
 g:=pValue.b;
end;
function TVector3.GetBRB:TVector3;
begin
 result.r:=b;
 result.g:=r;
 result.b:=b;
end;
function TVector3.GetBG:TVector2;
begin
 result.r:=b;
 result.g:=g;
end;
procedure TVector3.SetBG(const pValue:TVector2);
begin
 b:=pValue.r;
 g:=pValue.g;
end;
function TVector3.GetBGR:TVector3;
begin
 result.r:=b;
 result.g:=g;
 result.b:=r;
end;
procedure TVector3.SetBGR(const pValue:TVector3);
begin
 b:=pValue.r;
 g:=pValue.g;
 r:=pValue.b;
end;
function TVector3.GetBGG:TVector3;
begin
 result.r:=b;
 result.g:=g;
 result.b:=g;
end;
function TVector3.GetBGB:TVector3;
begin
 result.r:=b;
 result.g:=g;
 result.b:=b;
end;
function TVector3.GetBB:TVector2;
begin
 result.r:=b;
 result.g:=b;
end;
function TVector3.GetBBR:TVector3;
begin
 result.r:=b;
 result.g:=b;
 result.b:=r;
end;
function TVector3.GetBBG:TVector3;
begin
 result.r:=b;
 result.g:=b;
 result.b:=g;
end;
function TVector3.GetBBB:TVector3;
begin
 result.r:=b;
 result.g:=b;
 result.b:=b;
end;
function TVector3.GetSS:TVector2;
begin
 result.s:=s;
 result.t:=s;
end;
function TVector3.GetSSS:TVector3;
begin
 result.s:=s;
 result.t:=s;
 result.p:=s;
end;
function TVector3.GetSST:TVector3;
begin
 result.s:=s;
 result.t:=s;
 result.p:=t;
end;
function TVector3.GetSSP:TVector3;
begin
 result.s:=s;
 result.t:=s;
 result.p:=p;
end;
function TVector3.GetST:TVector2;
begin
 result.s:=s;
 result.t:=t;
end;
procedure TVector3.SetST(const pValue:TVector2);
begin
 s:=pValue.s;
 t:=pValue.t;
end;
function TVector3.GetSTS:TVector3;
begin
 result.s:=s;
 result.t:=t;
 result.p:=s;
end;
function TVector3.GetSTT:TVector3;
begin
 result.s:=s;
 result.t:=t;
 result.p:=t;
end;
function TVector3.GetSTP:TVector3;
begin
 result.s:=s;
 result.t:=t;
 result.p:=p;
end;
procedure TVector3.SetSTP(const pValue:TVector3);
begin
 s:=pValue.s;
 t:=pValue.t;
 p:=pValue.p;
end;
function TVector3.GetSP:TVector2;
begin
 result.s:=s;
 result.t:=p;
end;
procedure TVector3.SetSP(const pValue:TVector2);
begin
 s:=pValue.s;
 p:=pValue.t;
end;
function TVector3.GetSPS:TVector3;
begin
 result.s:=s;
 result.t:=p;
 result.p:=s;
end;
function TVector3.GetSPT:TVector3;
begin
 result.s:=s;
 result.t:=p;
 result.p:=t;
end;
procedure TVector3.SetSPT(const pValue:TVector3);
begin
 s:=pValue.s;
 p:=pValue.t;
 t:=pValue.p;
end;
function TVector3.GetSPP:TVector3;
begin
 result.s:=s;
 result.t:=p;
 result.p:=p;
end;
function TVector3.GetTS:TVector2;
begin
 result.s:=t;
 result.t:=s;
end;
procedure TVector3.SetTS(const pValue:TVector2);
begin
 t:=pValue.s;
 s:=pValue.t;
end;
function TVector3.GetTSS:TVector3;
begin
 result.s:=t;
 result.t:=s;
 result.p:=s;
end;
function TVector3.GetTST:TVector3;
begin
 result.s:=t;
 result.t:=s;
 result.p:=t;
end;
function TVector3.GetTSP:TVector3;
begin
 result.s:=t;
 result.t:=s;
 result.p:=p;
end;
procedure TVector3.SetTSP(const pValue:TVector3);
begin
 t:=pValue.s;
 s:=pValue.t;
 p:=pValue.p;
end;
function TVector3.GetTT:TVector2;
begin
 result.s:=t;
 result.t:=t;
end;
function TVector3.GetTTS:TVector3;
begin
 result.s:=t;
 result.t:=t;
 result.p:=s;
end;
function TVector3.GetTTT:TVector3;
begin
 result.s:=t;
 result.t:=t;
 result.p:=t;
end;
function TVector3.GetTTP:TVector3;
begin
 result.s:=t;
 result.t:=t;
 result.p:=p;
end;
function TVector3.GetTP:TVector2;
begin
 result.s:=t;
 result.t:=p;
end;
procedure TVector3.SetTP(const pValue:TVector2);
begin
 t:=pValue.s;
 p:=pValue.t;
end;
function TVector3.GetTPS:TVector3;
begin
 result.s:=t;
 result.t:=p;
 result.p:=s;
end;
procedure TVector3.SetTPS(const pValue:TVector3);
begin
 t:=pValue.s;
 p:=pValue.t;
 s:=pValue.p;
end;
function TVector3.GetTPT:TVector3;
begin
 result.s:=t;
 result.t:=p;
 result.p:=t;
end;
function TVector3.GetTPP:TVector3;
begin
 result.s:=t;
 result.t:=p;
 result.p:=p;
end;
function TVector3.GetPS:TVector2;
begin
 result.s:=p;
 result.t:=s;
end;
procedure TVector3.SetPS(const pValue:TVector2);
begin
 p:=pValue.s;
 s:=pValue.t;
end;
function TVector3.GetPSS:TVector3;
begin
 result.s:=p;
 result.t:=s;
 result.p:=s;
end;
function TVector3.GetPST:TVector3;
begin
 result.s:=p;
 result.t:=s;
 result.p:=t;
end;
procedure TVector3.SetPST(const pValue:TVector3);
begin
 p:=pValue.s;
 s:=pValue.t;
 t:=pValue.p;
end;
function TVector3.GetPSP:TVector3;
begin
 result.s:=p;
 result.t:=s;
 result.p:=p;
end;
function TVector3.GetPT:TVector2;
begin
 result.s:=p;
 result.t:=t;
end;
procedure TVector3.SetPT(const pValue:TVector2);
begin
 p:=pValue.s;
 t:=pValue.t;
end;
function TVector3.GetPTS:TVector3;
begin
 result.s:=p;
 result.t:=t;
 result.p:=s;
end;
procedure TVector3.SetPTS(const pValue:TVector3);
begin
 p:=pValue.s;
 t:=pValue.t;
 s:=pValue.p;
end;
function TVector3.GetPTT:TVector3;
begin
 result.s:=p;
 result.t:=t;
 result.p:=t;
end;
function TVector3.GetPTP:TVector3;
begin
 result.s:=p;
 result.t:=t;
 result.p:=p;
end;
function TVector3.GetPP:TVector2;
begin
 result.s:=p;
 result.t:=p;
end;
function TVector3.GetPPS:TVector3;
begin
 result.s:=p;
 result.t:=p;
 result.p:=s;
end;
function TVector3.GetPPT:TVector3;
begin
 result.s:=p;
 result.t:=p;
 result.p:=t;
end;
function TVector3.GetPPP:TVector3;
begin
 result.s:=p;
 result.t:=p;
 result.p:=p;
end;
