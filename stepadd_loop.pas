unit stepadd_loop;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin;

type

  { TfrmStepAdd_Loop }

  TfrmStepAdd_Loop = class(TForm)
    OkButton: TButton;
    CancelButton: TButton;
    chkCapIEneI: TCheckBox;
    lblLoopCount: TLabel;
    lblCapIEneI: TLabel;
    edtLoopCount: TSpinEdit;
  private

  public

  end;

var
  frmStepAdd_Loop: TfrmStepAdd_Loop;

implementation

{$R *.lfm}

end.

