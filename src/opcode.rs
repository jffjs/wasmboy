#[allow(non_camel_case_types)]
#[derive(Debug, FromPrimitive)]
pub enum Opcode {
    NOP = 0x00,
    LDBCnn = 0x01,
    LD_BC_A = 0x02,
    INCBC = 0x03,
    INCB = 0x04,
    DECB = 0x05,
    LDBn = 0x06,
    RLCA = 0x07,
    LD_nn_SP = 0x08,
    ADDHLBC = 0x09,
    LDA_BC_ = 0x0a,
    DECBC = 0x0b,
    INCC = 0x0c,
    DECC = 0x0d,
    LDCn = 0x0e,
    RRCA = 0x0f,
    STOP = 0x10,
    LDDEnn = 0x11,
    LD_DE_A = 0x12,
    INCDE = 0x13,
    INCD = 0x14,
    DECD = 0x15,
    LDDn = 0x16,
    RLA = 0x17,
    JRn = 0x18,
    ADDHLDE = 0x19,
    LDA_DE_ = 0x1a,
    DECDE = 0x1b,
    INCE = 0x1c,
    DECE = 0x1d,
    LDEn = 0x1e,
    RRA = 0x1f,
    JRNZn = 0x20,
    LDHLnn = 0x21,
    LDI_HL_A = 0x22,
    INCHL = 0x23,
    INCH = 0x24,
    DECH = 0x25,
    LDHn = 0x26,
    DAA = 0x27,
    JRZn = 0x28,
    ADDHLHL = 0x29,
    LDIA_HL_ = 0x2a,
    DECHL = 0x2b,
    INCL = 0x2c,
    DECL = 0x2d,
    LDLn = 0x2e,
    CPL = 0x2f,
    JRNCn = 0x30,
    LDSPnn = 0x31,
    LDD_HL_A = 0x32,
    INCSP = 0x33,
    INC_HL_ = 0x34,
    DEC_HL_ = 0x35,
    LD_HL_n = 0x36,
    SCF = 0x37,
    JRCn = 0x38,
    ADDHLSP = 0x39,
    LDDA_HL_ = 0x3a,
    DECSP = 0x3b,
    INCA = 0x3c,
    DECA = 0x3d,
    LDAn = 0x3e,
    CCF = 0x3f,
    LDBB = 0x40,
    LDBC = 0x41,
    LDBD = 0x42,
    LDBE = 0x43,
    LDBH = 0x44,
    LDBL = 0x45,
    LDB_HL_ = 0x46,
    LDBA = 0x47,
    LDCB = 0x48,
    LDCC = 0x49,
    LDCD = 0x4a,
    LDCE = 0x4b,
    LDCH = 0x4c,
    LDCL = 0x4d,
    LDC_HL_ = 0x4e,
    LDCA = 0x4f,
    LDDB = 0x50,
    LDDC = 0x51,
    LDDD = 0x52,
    LDDE = 0x53,
    LDDH = 0x54,
    LDDL = 0x55,
    LDD_HL_ = 0x56,
    LDDA = 0x57,
    LDEB = 0x58,
    LDEC = 0x59,
    LDED = 0x5a,
    LDEE = 0x5b,
    LDEH = 0x5c,
    LDEL = 0x5d,
    LDE_HL_ = 0x5e,
    LDEA = 0x5f,
    LDHB = 0x60,
    LDHC = 0x61,
    LDHD = 0x62,
    LDHE = 0x63,
    LDHH = 0x64,
    LDHL = 0x65,
    LDH_HL_ = 0x66,
    LDHA = 0x67,
    LDLB = 0x68,
    LDLC = 0x69,
    LDLD = 0x6a,
    LDLE = 0x6b,
    LDLH = 0x6c,
    LDLL = 0x6d,
    LDL_HL_ = 0x6e,
    LDLA = 0x6f,
    LD_HL_B = 0x70,
    LD_HL_C = 0x71,
    LD_HL_D = 0x72,
    LD_HL_E = 0x73,
    LD_HL_H = 0x74,
    LD_HL_L = 0x75,
    HALT = 0x76,
    LD_HL_A = 0x77,
    LDAB = 0x78,
    LDAC = 0x79,
    LDAD = 0x7a,
    LDAE = 0x7b,
    LDAH = 0x7c,
    LDAL = 0x7d,
    LDA_HL_ = 0x7e,
    LDAA = 0x7f,
    ADDAB = 0x80,
    ADDAC = 0x81,
    ADDAD = 0x82,
    ADDAE = 0x83,
    ADDAH = 0x84,
    ADDAL = 0x85,
    ADDA_HL_ = 0x86,
    ADDAA = 0x87,
    ADCAB = 0x88,
    ADCAC = 0x89,
    ADCAD = 0x8a,
    ADCAE = 0x8b,
    ADCAH = 0x8c,
    ADCAL = 0x8d,
    ADCA_HL_ = 0x8e,
    ADCAA = 0x8f,
    SUBAB = 0x90,
    SUBAC = 0x91,
    SUBAD = 0x92,
    SUBAE = 0x93,
    SUBAH = 0x94,
    SUBAL = 0x95,
    SUBA_HL_ = 0x96,
    SUBAA = 0x97,
    SBCAB = 0x98,
    SBCAC = 0x99,
    SBCAD = 0x9a,
    SBCAE = 0x9b,
    SBCAH = 0x9c,
    SBCAL = 0x9d,
    SBCA_HL_ = 0x9e,
    SBCAA = 0x9f,
    ANDB = 0xa0,
    ANDC = 0xa1,
    ANDD = 0xa2,
    ANDE = 0xa3,
    ANDH = 0xa4,
    ANDL = 0xa5,
    AND_HL_ = 0xa6,
    ANDA = 0xa7,
    XORB = 0xa8,
    XORC = 0xa9,
    XORD = 0xaa,
    XORE = 0xab,
    XORH = 0xac,
    XORL = 0xad,
    XOR_HL_ = 0xae,
    XORA = 0xaf,
    ORB = 0xb0,
    ORC = 0xb1,
    ORD = 0xb2,
    ORE = 0xb3,
    ORH = 0xb4,
    ORL = 0xb5,
    OR_HL_ = 0xb6,
    ORA = 0xb7,
    CPB = 0xb8,
    CPC = 0xb9,
    CPD = 0xba,
    CPE = 0xbb,
    CPH = 0xbc,
    CP_L = 0xbd, // CPL is taken
    CP_HL_ = 0xbe,
    CPA = 0xbf,
    RETNZ = 0xc0,
    POPBC = 0xc1,
    JPNZnn = 0xc2,
    JPnn = 0xc3,
    CALLNZnn = 0xc4,
    PUSHBC = 0xc5,
    ADDAn = 0xc6,
    RST0 = 0xc7,
    RETZ = 0xc8,
    RET = 0xc9,
    JPZnn = 0xca,
    ExtOps = 0xcb,
    CALLZnn = 0xcc,
    CALLnn = 0xcd,
    ADCAn = 0xce,
    RST8 = 0xcf,
    RETNC = 0xd0,
    POPDE = 0xd1,
    JPNCnn = 0xd2,
    // 0xd3, // not supported
    CALLNCnn = 0xd4,
    PUSHDE = 0xd5,
    SUBAn = 0xd6,
    RST10 = 0xd7,
    RETC = 0xd8,
    RETI = 0xd9,
    JPCnn = 0xda,
    // 0xdb, // not supported
    CALLCnn = 0xdc,
    // 0xdd, // not supported
    SBCAn = 0xde,
    RST18 = 0xdf,
    LDH_n_A = 0xe0,
    POPHL = 0xe1,
    LD_C_A = 0xe2,
    // 0xe3, // not supported
    // 0xe4, // not supported
    PUSHHL = 0xe5,
    ANDn = 0xe6,
    RST20 = 0xe7,
    ADDSPn = 0xe8,
    JP_HL_ = 0xe9,
    LD_nn_A = 0xea,
    // 0xeb, // not supported
    // 0xec, // not supported
    // 0xed, // not supported
    XORn = 0xee,
    RST28 = 0xef,
    LDHA_n_ = 0xf0,
    POPAF = 0xf1,
    // 0xf2, // not supported
    DI = 0xf3,
    // 0xf4, // not supported
    PUSHAF = 0xf5,
    ORn = 0xf6,
    RST30 = 0xf7,
    LDHLSPn = 0xf8,
    LDSPHL = 0xf9,
    LDA_nn_ = 0xfa,
    EI = 0xfb,
    // 0xfc, // not supported
    // 0xfd, // not supported
    CPn = 0xfe,
    RST38 = 0xff,
}

#[allow(non_camel_case_types)]
#[derive(FromPrimitive)]
pub enum ExtOpcode {
    RLCB = 0x00,
    RLCC = 0x01,
    RLCD = 0x02,
    RLCE = 0x03,
    RLCH = 0x04,
    RLCL = 0x05,
    RLC_HL_ = 0x06,
    RLCA = 0x07,
    RRCB = 0x08,
    RRCC = 0x09,
    RRCD = 0x0a,
    RRCE = 0x0b,
    RRCH = 0x0c,
    RRCL = 0x0d,
    RRC_HL_ = 0x0e,
    RRC_A = 0x0f,
    RLB = 0x10,
    RLC = 0x11,
    RLD = 0x12,
    RLE = 0x13,
    RLH = 0x14,
    RLL = 0x15,
    RL_HL_ = 0x16,
    RLA = 0x17,
    RRB = 0x18,
    RRC = 0x19,
    RRD = 0x1a,
    RRE = 0x1b,
    RRH = 0x1c,
    RRL = 0x1d,
    RR_HL_ = 0x1e,
    RRA = 0x1f,
    SLAB = 0x20,
    SLAC = 0x21,
    SLAD = 0x22,
    SLAE = 0x23,
    SLAH = 0x24,
    SLAL = 0x25,
    SLA_HL_ = 0x26,
    SLAA = 0x27,
    SRAB = 0x28,
    SRAC = 0x29,
    SRAD = 0x2a,
    SRAE = 0x2b,
    SRAH = 0x2c,
    SRAL = 0x2d,
    SRA_HL = 0x2e,
    SRAA = 0x2f,
    SWAPB = 0x30,
    SWAPC = 0x31,
    SWAPD = 0x32,
    SWAPE = 0x33,
    SWAPH = 0x34,
    SWAPL = 0x35,
    SWAP_HL_ = 0x36,
    SWAPA = 0x37,
    SRLB = 0x38,
    SRLC = 0x39,
    SRLD = 0x3a,
    SRLE = 0x3b,
    SRLH = 0x3c,
    SRLL = 0x3d,
    SRL_HL_ = 0x3e,
    SRLA = 0x3f,
    BIT0B = 0x40,
    BIT0C = 0x41,
    BIT0D = 0x42,
    BIT0E = 0x43,
    BIT0H = 0x44,
    BIT0L = 0x45,
    BIT0_HL_ = 0x46,
    BIT0A = 0x47,
    BIT1B = 0x48,
    BIT1C = 0x49,
    BIT1D = 0x4a,
    BIT1E = 0x4b,
    BIT1H = 0x4c,
    BIT1L = 0x4d,
    BIT1_HL_ = 0x4e,
    BIT1A = 0x4f,
    BIT2B = 0x50,
    BIT2C = 0x51,
    BIT2D = 0x52,
    BIT2E = 0x53,
    BIT2H = 0x54,
    BIT2L = 0x55,
    BIT2_HL_ = 0x56,
    BIT2A = 0x57,
    BIT3B = 0x58,
    BIT3C = 0x59,
    BIT3D = 0x5a,
    BIT3E = 0x5b,
    BIT3H = 0x5c,
    BIT3L = 0x5d,
    BIT3_HL_ = 0x5e,
    BIT3A = 0x5f,
    BIT4B = 0x60,
    BIT4C = 0x61,
    BIT4D = 0x62,
    BIT4E = 0x63,
    BIT4H = 0x64,
    BIT4L = 0x65,
    BIT4_HL_ = 0x66,
    BIT4A = 0x67,
    BIT5B = 0x68,
    BIT5C = 0x69,
    BIT5D = 0x6a,
    BIT5E = 0x6b,
    BIT5H = 0x6c,
    BIT5L = 0x6d,
    BIT5_HL_ = 0x6e,
    BIT5A = 0x6f,
    BIT6B = 0x70,
    BIT6C = 0x71,
    BIT6D = 0x72,
    BIT6E = 0x73,
    BIT6H = 0x74,
    BIT6L = 0x75,
    BIT6_HL_ = 0x76,
    BIT6A = 0x77,
    BIT7B = 0x78,
    BIT7C = 0x79,
    BIT7D = 0x7a,
    BIT7E = 0x7b,
    BIT7H = 0x7c,
    BIT7L = 0x7d,
    BIT7_HL_ = 0x7e,
    BIT7A = 0x7f,
    RES0B = 0x80,
    RES0C = 0x81,
    RES0D = 0x82,
    RES0E = 0x83,
    RES0H = 0x84,
    RES0L = 0x85,
    RES0_HL_ = 0x86,
    RES0A = 0x87,
    RES1B = 0x88,
    RES1C = 0x89,
    RES1D = 0x8a,
    RES1E = 0x8b,
    RES1H = 0x8c,
    RES1L = 0x8d,
    RES1_HL_ = 0x8e,
    RES1A = 0x8f,
    RES2B = 0x90,
    RES2C = 0x91,
    RES2D = 0x92,
    RES2E = 0x93,
    RES2H = 0x94,
    RES2L = 0x95,
    RES2_HL_ = 0x96,
    RES2A = 0x97,
    RES3B = 0x98,
    RES3C = 0x99,
    RES3D = 0x9a,
    RES3E = 0x9b,
    RES3H = 0x9c,
    RES3L = 0x9d,
    RES3_HL_ = 0x9e,
    RES3A = 0x9f,
    RES4B = 0xa0,
    RES4C = 0xa1,
    RES4D = 0xa2,
    RES4E = 0xa3,
    RES4H = 0xa4,
    RES4L = 0xa5,
    RES4_HL_ = 0xa6,
    RES4A = 0xa7,
    RES5B = 0xa8,
    RES5C = 0xa9,
    RES5D = 0xaa,
    RES5E = 0xab,
    RES5H = 0xac,
    RES5L = 0xad,
    RES5_HL_ = 0xae,
    RES5A = 0xaf,
    RES6B = 0xb0,
    RES6C = 0xb1,
    RES6D = 0xb2,
    RES6E = 0xb3,
    RES6H = 0xb4,
    RES6L = 0xb5,
    RES6_HL_ = 0xb6,
    RES6A = 0xb7,
    RES7B = 0xb8,
    RES7C = 0xb9,
    RES7D = 0xba,
    RES7E = 0xbb,
    RES7H = 0xbc,
    RES7L = 0xbd,
    RES7_HL_ = 0xbe,
    RES7A = 0xbf,
    SET0B = 0xc0,
    SET0C = 0xc1,
    SET0D = 0xc2,
    SET0E = 0xc3,
    SET0H = 0xc4,
    SET0L = 0xc5,
    SET0_HL_ = 0xc6,
    SET0A = 0xc7,
    SET1B = 0xc8,
    SET1C = 0xc9,
    SET1D = 0xca,
    SET1E = 0xcb,
    SET1H = 0xcc,
    SET1L = 0xcd,
    SET1_HL_ = 0xce,
    SET1A = 0xcf,
    SET2B = 0xd0,
    SET2C = 0xd1,
    SET2D = 0xd2,
    SET2E = 0xd3,
    SET2H = 0xd4,
    SET2L = 0xd5,
    SET2_HL_ = 0xd6,
    SET2A = 0xd7,
    SET3B = 0xd8,
    SET3C = 0xd9,
    SET3D = 0xda,
    SET3E = 0xdb,
    SET3H = 0xdc,
    SET3L = 0xdd,
    SET3_HL_ = 0xde,
    SET3A = 0xdf,
    SET4B = 0xe0,
    SET4C = 0xe1,
    SET4D = 0xe2,
    SET4E = 0xe3,
    SET4H = 0xe4,
    SET4L = 0xe5,
    SET4_HL_ = 0xe6,
    SET4A = 0xe7,
    SET5B = 0xe8,
    SET5C = 0xe9,
    SET5D = 0xea,
    SET5E = 0xeb,
    SET5H = 0xec,
    SET5L = 0xed,
    SET5_HL_ = 0xee,
    SET5A = 0xef,
    SET6B = 0xf0,
    SET6C = 0xf1,
    SET6D = 0xf2,
    SET6E = 0xf3,
    SET6H = 0xf4,
    SET6L = 0xf5,
    SET6_HL_ = 0xf6,
    SET6A = 0xf7,
    SET7B = 0xf8,
    SET7C = 0xf9,
    SET7D = 0xfa,
    SET7E = 0xfb,
    SET7H = 0xfc,
    SET7L = 0xfd,
    SET7_HL_ = 0xfe,
    SET7A = 0xff,
}
