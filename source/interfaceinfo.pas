unit InterfaceInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, Windows, Winsock;

//procedure GetNetworkedDrives(AList: TStrings);
function EnumInterfaces(AList: TStrings): Boolean;

function WSAIoctl(s: TSocket; cmd: DWORD; lpInBuffer: PCHAR; dwInBufferLen:
  DWORD;
  lpOutBuffer: PCHAR; dwOutBufferLen: DWORD;
  lpdwOutBytesReturned: LPDWORD;
  lpOverLapped: POINTER;
  lpOverLappedRoutine: POINTER): Integer; stdcall; external 'WS2_32.DLL';

const
  SIO_GET_INTERFACE_LIST = $4004747F;
  IFF_UP = $00000001;
  IFF_BROADCAST = $00000002;
  IFF_LOOPBACK = $00000004;
  IFF_POINTTOPOINT = $00000008;
  IFF_MULTICAST = $00000010;

type
  sockaddr_gen = packed record
    AddressIn: sockaddr_in;
    filler: packed array[0..7] of char;
  end;

type
  INTERFACE_INFO = packed record
    iiFlags: u_long;
    iiAddress: sockaddr_gen;
    iiBroadcastAddress: sockaddr_gen;
    iiNetmask: sockaddr_gen;
  end;

implementation

function EnumInterfaces(AList: TStrings): Boolean;
var
  s: TSocket;
  wsaD: WSADATA;
  NumInterfaces: Integer;
  BytesReturned, SetFlags: u_long;
  pAddrInet: SOCKADDR_IN;
  pAddrString: PCHAR;
  PtrA: pointer;
  Buffer: array[0..20] of INTERFACE_INFO;
  i: Integer;
  sInt: String;
begin
  result := false;
  WSAStartup($0101, wsaD);

  s := Socket(AF_INET, SOCK_STREAM, 0);
  if (s = INVALID_SOCKET) then
    exit;

  try
    PtrA := @bytesReturned;
    if (WSAIoCtl(s, SIO_GET_INTERFACE_LIST, nil, 0, @Buffer, 1024, PtrA, nil, nil) <> SOCKET_ERROR)
    then begin
      NumInterfaces := BytesReturned div SizeOf(INTERFACE_INFO);

      for i := 0 to NumInterfaces - 1 do {Loop trough all interfaces}
      begin
        sInt := '';
        pAddrInet := Buffer[i].iiAddress.addressIn;
        pAddrString := inet_ntoa(pAddrInet.sin_addr);
//        sInt := sInt + ' Name: ' + GetNetworkName(pAddrString);
        sInt := sInt + ' IP Address: ' + pAddrString + ',';
        pAddrInet := Buffer[i].iiNetMask.addressIn;
        pAddrString := inet_ntoa(pAddrInet.sin_addr);
        sInt := sInt + ' Subnet Mask: ' + pAddrString + LineEnding;
        AList.Add(sInt);
      end;
    end;
  except
  end;
  CloseSocket(s);
  WSACleanUp;
  result := true;
end;

(* -- not working - computer hangs
// http://www.angusj.com/delphitips/netdrives.php
procedure GetNetworkedDrives(AList: TStrings);

  procedure EnumNetworkDrives(pnr: PNetResource);
  var
    hEnum: THandle;
    i, enumRes, count, BufferSize: DWORD;
    buffer: pointer;
  begin
    BufferSize := $4000; //ie: use a 16kb buffer
    buffer := nil;  //just in case memory allocation fails
    if WNetOpenEnum(RESOURCE_GLOBALNET,
      RESOURCETYPE_DISK, 0, pnr, hEnum) = ERROR_SUCCESS then
    try
      GetMem(buffer, BufferSize);
      while true do
      begin
        count := dword(-1); //ie: get as many items possible
        enumRes := WNetEnumResource(hEnum, count, buffer, BufferSize);

        //break if either no more items found or an error occurs...
        if (enumRes <> ERROR_SUCCESS) then break;

        pnr := buffer; //reuse the pnr pointer
        for i := 1 to count do
        begin
          if (pnr^.dwDisplayType = RESOURCEDISPLAYTYPE_DOMAIN or RESOURCEDISPLAYTYPE_SERVER) and
             (pnr^.dwType = RESOURCETYPE_DISK)
          then
            AList.Add(pnr^.lpRemoteName);
          //recursive function call...
          if (pnr^.dwUsage and RESOURCEUSAGE_CONTAINER) > 0 then
            EnumNetworkDrives(pnr);
          inc(longint(pnr), SizeOf(TNetResource));
        end;
      end;

    finally
      FreeMem(buffer);
      WNetCloseEnum(hEnum);
    end;
  end;

begin
  if AList = nil then exit;
  //simply call the nested recursive function
  //passing nil to starting at the network root ...
  EnumNetworkDrives(nil);
end;
*)

end.
