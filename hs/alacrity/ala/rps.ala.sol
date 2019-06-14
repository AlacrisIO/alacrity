pragma solidity ^0.5.2;

import "../sol/stdlib.sol";

contract ALAFactory {
  function make(address payable partA, address payable partB, uint256 Declassify_13, uint256 Declassify_14, bytes memory Declassify_15) public payable returns (ALAContract _ctc) {
    ALAContract ctc = new ALAContract(partA, partB);
    ctc.msg0_m.value(msg.value)(partA, partB, Declassify_13, Declassify_14, Declassify_15);
    return ctc; } }

contract ALAContract is Stdlib {
  bytes32 current_state;
  
  constructor(address payable partA, address payable partB) public payable {
    current_state = keccak256(abi.encode(0, partA, partB)); }
  
  event msg0_evt(uint256 Declassify_13, uint256 Declassify_14, bytes Declassify_15);
  function msg0_m(address payable partA, address payable partB, uint256 Declassify_13, uint256 Declassify_14, bytes calldata Declassify_15) external payable {
    require(current_state == keccak256(abi.encode(0, partA, partB)));
    require(msg.sender == partA);
    current_state = keccak256(abi.encode(1, partA, partB, Declassify_13, Declassify_14, Declassify_15));
    emit msg0_evt(Declassify_13, Declassify_14, Declassify_15); }
  
  event msg1_evt(uint256 Declassify_23);
  function msg1_m(address payable partA, address payable partB, uint256 Declassify_13, uint256 Declassify_14, bytes calldata Declassify_15, uint256 Declassify_23) external payable {
    require(current_state == keccak256(abi.encode(1, partA, partB, Declassify_13, Declassify_14, Declassify_15)));
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
    current_state = keccak256(abi.encode(2, partA, partB, Declassify_13, Declassify_14, Declassify_15, Declassify_23));
    emit msg1_evt(Declassify_23); }
  
  event msg2_evt(bytes Declassify_34);
  function msg2_m(address payable partA, address payable partB, uint256 Declassify_13, uint256 Declassify_14, bytes calldata Declassify_15, uint256 Declassify_23, bytes calldata Declassify_34) external payable {
    require(current_state == keccak256(abi.encode(2, partA, partB, Declassify_13, Declassify_14, Declassify_15, Declassify_23)));
    require(msg.sender == partA);
    bytes memory PrimApp_35 = keccak256(Declassify_34);
    bool PrimApp_36 = Declassify_15 == PrimApp_35;
    require(PrimApp_36);
    bytes memory PrimApp_38 = ALA_BCAT_RIGHT(Declassify_34);
    bool PrimApp_39 = true;
    bool PrimApp_40 = PrimApp_39 ? false : true;
    bool PrimApp_41 = PrimApp_38 == 0;
    bool PrimApp_42 = PrimApp_38 == 1;
    bool PrimApp_43 = PrimApp_38 == 2;
    bool PrimApp_44 = PrimApp_42 ? true : PrimApp_43;
    bool PrimApp_45 = PrimApp_41 ? true : PrimApp_44;
    bool PrimApp_46 = PrimApp_40 ? false : true;
    bool PrimApp_47 = PrimApp_46 ? true : PrimApp_45;
    require(PrimApp_47);
    bool PrimApp_49 = PrimApp_38 == 0;
    bool PrimApp_50 = PrimApp_38 == 1;
    bool PrimApp_51 = PrimApp_38 == 2;
    bool PrimApp_52 = PrimApp_50 ? true : PrimApp_51;
    bool PrimApp_53 = PrimApp_49 ? true : PrimApp_52;
    bool PrimApp_54 = Declassify_23 == 0;
    bool PrimApp_55 = Declassify_23 == 1;
    bool PrimApp_56 = Declassify_23 == 2;
    bool PrimApp_57 = PrimApp_55 ? true : PrimApp_56;
    bool PrimApp_58 = PrimApp_54 ? true : PrimApp_57;
    bool PrimApp_59 = PrimApp_53 ? PrimApp_58 : false;
    uint256 PrimApp_60 = 4 - Declassify_23;
    uint256 PrimApp_61 = PrimApp_38 + PrimApp_60;
    uint256 PrimApp_62 = PrimApp_61 % 3;
    uint256 PureIf_63 = PrimApp_58 ? 0 : 1;
    uint256 PureIf_64 = PrimApp_53 ? 2 : PureIf_63;
    uint256 PureIf_65 = PrimApp_59 ? PrimApp_62 : PureIf_64;
    bool PrimApp_66 = PureIf_65 == 0;
    bool PrimApp_67 = PureIf_65 == 1;
    bool PrimApp_68 = PureIf_65 == 2;
    bool PrimApp_69 = PrimApp_67 ? true : PrimApp_68;
    bool PrimApp_70 = PrimApp_66 ? true : PrimApp_69;
    require(PrimApp_70);
    bool PrimApp_72 = PureIf_65 == 2;
    bool PrimApp_73 = PrimApp_38 == 0;
    bool PrimApp_74 = PrimApp_38 == 1;
    bool PrimApp_75 = PrimApp_38 == 2;
    bool PrimApp_76 = PrimApp_74 ? true : PrimApp_75;
    bool PrimApp_77 = PrimApp_73 ? true : PrimApp_76;
    bool PrimApp_78 = PrimApp_72 ? false : true;
    bool PrimApp_79 = PrimApp_78 ? true : PrimApp_77;
    require(PrimApp_79);
    bool PrimApp_81 = PureIf_65 == 0;
    bool PrimApp_82 = Declassify_23 == 0;
    bool PrimApp_83 = Declassify_23 == 1;
    bool PrimApp_84 = Declassify_23 == 2;
    bool PrimApp_85 = PrimApp_83 ? true : PrimApp_84;
    bool PrimApp_86 = PrimApp_82 ? true : PrimApp_85;
    bool PrimApp_87 = PrimApp_81 ? false : true;
    bool PrimApp_88 = PrimApp_87 ? true : PrimApp_86;
    require(PrimApp_88);
    bool PrimApp_90 = PureIf_65 == 2;
    uint256 PrimApp_91 = 2 * Declassify_13;
    uint256 PrimApp_92 = PrimApp_91 + Declassify_14;
    bool PrimApp_93 = PureIf_65 == 0;
    uint256 PrimApp_94 = 2 * Declassify_13;
    uint256 PrimApp_95 = Declassify_13 + Declassify_14;
    uint256 PureIf_96 = PrimApp_93 ? Declassify_14 : PrimApp_95;
    uint256 PureIf_97 = PrimApp_93 ? PrimApp_94 : Declassify_13;
    uint256 PureIf_98 = PrimApp_90 ? PrimApp_92 : PureIf_96;
    uint256 PureIf_99 = PrimApp_90 ? 0 : PureIf_97;
    partA.transfer(PureIf_98);
    partB.transfer(PureIf_99);
    selfdestruct();
    emit msg2_evt(Declassify_34); } }