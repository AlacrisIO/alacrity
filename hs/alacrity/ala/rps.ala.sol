pragma solidity ^0.5.2;


contract ALAFactory {
  function make(address payable partA, address payable partB, uint256 Declassify_13, uint256 Declassify_14, uint256 Declassify_15) public payable returns (ALAContract _ctc) {
    ALAContract ctc = new ALAContract(partA, partB);
    ctc.msg0_m.value(msg.value)(partA, partB, Declassify_13, Declassify_14, Declassify_15);
    return ctc; } }

contract ALAContract {
  uint256 current_state;
  
  constructor(address payable partA, address payable partB) public payable {
    current_state = uint256(keccak256( abi.encode(0, partA, partB) )); }
  
  event msg0_evt(uint256 Declassify_13, uint256 Declassify_14, uint256 Declassify_15);
  function msg0_m(address payable partA, address payable partB, uint256 Declassify_13, uint256 Declassify_14, uint256 Declassify_15) external payable {
    require(current_state == uint256(keccak256( abi.encode(0, partA, partB) )));
    require(msg.sender == partA);
    current_state = uint256(keccak256( abi.encode(1, partA, partB, Declassify_13, Declassify_14, Declassify_15) ));
    emit msg0_evt(Declassify_13, Declassify_14, Declassify_15); }
  
  event msg1_evt(uint256 Declassify_23);
  function msg1_m(address payable partA, address payable partB, uint256 Declassify_13, uint256 Declassify_14, uint256 Declassify_15, uint256 Declassify_23) external payable {
    require(current_state == uint256(keccak256( abi.encode(1, partA, partB, Declassify_13, Declassify_14, Declassify_15) )));
    require(msg.sender == partB);
    bool PrimApp_24 = true;
    bool PrimApp_25 = PrimApp_24 ? false : true;
    bool PrimApp_26 = Declassify_23 == 0;
    bool PrimApp_27 = Declassify_23 == 1;
    bool PrimApp_28 = Declassify_23 == 2;
    bool PrimApp_29 = PrimApp_27 ? true : PrimApp_28;
    bool PrimApp_30 = PrimApp_26 ? true : PrimApp_29;
    bool PrimApp_31 = PrimApp_25 ? false : true;
    bool PrimApp_32 = PrimApp_31 ? true : PrimApp_30;
    require(PrimApp_32);
    current_state = uint256(keccak256( abi.encode(2, partA, partB, Declassify_13, Declassify_14, Declassify_15, Declassify_23) ));
    emit msg1_evt(Declassify_23); }
  
  event msg2_evt(uint256 Declassify_34, uint256 Declassify_35);
  function msg2_m(address payable partA, address payable partB, uint256 Declassify_13, uint256 Declassify_14, uint256 Declassify_15, uint256 Declassify_23, uint256 Declassify_34, uint256 Declassify_35) external payable {
    require(current_state == uint256(keccak256( abi.encode(2, partA, partB, Declassify_13, Declassify_14, Declassify_15, Declassify_23) )));
    require(msg.sender == partA);
    bytes memory PrimApp_36 = abi.encode(Declassify_34, Declassify_35);
    uint256 PrimApp_37 = uint256(keccak256( abi.encode(PrimApp_36) ));
    bool PrimApp_38 = Declassify_15 == PrimApp_37;
    require(PrimApp_38);
    bool PrimApp_40 = true;
    bool PrimApp_41 = PrimApp_40 ? false : true;
    bool PrimApp_42 = Declassify_35 == 0;
    bool PrimApp_43 = Declassify_35 == 1;
    bool PrimApp_44 = Declassify_35 == 2;
    bool PrimApp_45 = PrimApp_43 ? true : PrimApp_44;
    bool PrimApp_46 = PrimApp_42 ? true : PrimApp_45;
    bool PrimApp_47 = PrimApp_41 ? false : true;
    bool PrimApp_48 = PrimApp_47 ? true : PrimApp_46;
    require(PrimApp_48);
    bool PrimApp_50 = Declassify_35 == 0;
    bool PrimApp_51 = Declassify_35 == 1;
    bool PrimApp_52 = Declassify_35 == 2;
    bool PrimApp_53 = PrimApp_51 ? true : PrimApp_52;
    bool PrimApp_54 = PrimApp_50 ? true : PrimApp_53;
    bool PrimApp_55 = Declassify_23 == 0;
    bool PrimApp_56 = Declassify_23 == 1;
    bool PrimApp_57 = Declassify_23 == 2;
    bool PrimApp_58 = PrimApp_56 ? true : PrimApp_57;
    bool PrimApp_59 = PrimApp_55 ? true : PrimApp_58;
    bool PrimApp_60 = PrimApp_54 ? PrimApp_59 : false;
    uint256 PrimApp_61 = 4 - Declassify_23;
    uint256 PrimApp_62 = Declassify_35 + PrimApp_61;
    uint256 PrimApp_63 = PrimApp_62 % 3;
    uint256 PureIf_64 = PrimApp_59 ? 0 : 1;
    uint256 PureIf_65 = PrimApp_54 ? 2 : PureIf_64;
    uint256 PureIf_66 = PrimApp_60 ? PrimApp_63 : PureIf_65;
    bool PrimApp_67 = PureIf_66 == 0;
    bool PrimApp_68 = PureIf_66 == 1;
    bool PrimApp_69 = PureIf_66 == 2;
    bool PrimApp_70 = PrimApp_68 ? true : PrimApp_69;
    bool PrimApp_71 = PrimApp_67 ? true : PrimApp_70;
    require(PrimApp_71);
    bool PrimApp_73 = PureIf_66 == 2;
    bool PrimApp_74 = Declassify_35 == 0;
    bool PrimApp_75 = Declassify_35 == 1;
    bool PrimApp_76 = Declassify_35 == 2;
    bool PrimApp_77 = PrimApp_75 ? true : PrimApp_76;
    bool PrimApp_78 = PrimApp_74 ? true : PrimApp_77;
    bool PrimApp_79 = PrimApp_73 ? false : true;
    bool PrimApp_80 = PrimApp_79 ? true : PrimApp_78;
    require(PrimApp_80);
    bool PrimApp_82 = PureIf_66 == 0;
    bool PrimApp_83 = Declassify_23 == 0;
    bool PrimApp_84 = Declassify_23 == 1;
    bool PrimApp_85 = Declassify_23 == 2;
    bool PrimApp_86 = PrimApp_84 ? true : PrimApp_85;
    bool PrimApp_87 = PrimApp_83 ? true : PrimApp_86;
    bool PrimApp_88 = PrimApp_82 ? false : true;
    bool PrimApp_89 = PrimApp_88 ? true : PrimApp_87;
    require(PrimApp_89);
    bool PrimApp_91 = PureIf_66 == 2;
    uint256 PrimApp_92 = 2 * Declassify_13;
    uint256 PrimApp_93 = PrimApp_92 + Declassify_14;
    bool PrimApp_94 = PureIf_66 == 0;
    uint256 PrimApp_95 = 2 * Declassify_13;
    uint256 PrimApp_96 = Declassify_13 + Declassify_14;
    uint256 PureIf_97 = PrimApp_94 ? Declassify_14 : PrimApp_96;
    uint256 PureIf_98 = PrimApp_94 ? PrimApp_95 : Declassify_13;
    uint256 PureIf_99 = PrimApp_91 ? PrimApp_93 : PureIf_97;
    uint256 PureIf_100 = PrimApp_91 ? 0 : PureIf_98;
    partA.transfer(PureIf_99);
    partB.transfer(PureIf_100);
    selfdestruct(address(0x02B463784Bc1a49f1647B47a19452aC420DFC65A));
    emit msg2_evt(Declassify_34, Declassify_35); } }