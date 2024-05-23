import os
import argparse
import copy
# memory size, in reality, the memory size should be 2^32, but for this lab, for the space resaon, we keep it as this large number, but the memory is still 32-bit addressable.
MemSize = 1000


def boolean_value(boo: bool) -> str:
    if boo == True:
        return '1'
    else:
        return '0'


def sign_extension(imm: str) -> str:
    result = imm
    signbit = "0"
    if result[0] == "1":
        signbit = "1"
    for i in range(32-len(imm)):
        result = signbit + result
    return result


def negate_str(str_binary: str) -> str:
    result = ""
    for i in range(32):
        if str_binary[i] == "1":
            result += "0"
        else:
            result += "1"
    return result

# convert string of 2's complement num to int number


def twos_complement_to_int(imm: str) -> int:
    result = 0
    extended = imm

    # if negative
    if imm[0] == "1":
        extended = sign_extension(imm)

        # negate 2's compliment
        tmp = int(negate_str(extended), 2)+1

        # truncate the int to 32bit
        tmp2 = tmp & 0xFFFFFFFF
        # get the relative addressing
        result = 0 - tmp2

    # if positive
    else:
        result = int(extended, 2)
    return result


# ALU for type R, operands are rs1 and rs2
ALU_type_R = ['ADD', 'SUB', 'XOR', 'OR', 'AND']

# ALU for type I, load and store instructions
# operands are rs1 and imm
ALU_type_I_L_S = ['ADDI', 'XORI', 'ORI', 'ANDI', 'LW', 'SW']


class InsMem(object):
    def __init__(self, name, ioDir):
        self.id = name

        with open(ioDir + "\\imem.txt") as im:
            self.IMem = [data.replace("\n", "") for data in im.readlines()]

    def readInstr(self, ReadAddress) -> str:
        # read instruction memory
        # return 32 bit hex val

        Instr_list = self.IMem[ReadAddress: ReadAddress+4]
        return ''.join(Instr_list)


class DataMem(object):
    def __init__(self, name, ioDir):
        self.id = name
        self.ioDir = ioDir
        with open(ioDir + "\\dmem.txt") as dm:
            self.DMem = [data.replace("\n", "") for data in dm.readlines()]

            # extend the DMem to 1000 to match the ExpectedResult
            while len(self.DMem) < 1000:
                self.DMem.append('00000000')

            # print("DMem size:", len(self.DMem))

    def readInstr(self, ReadAddress) -> str:
        # read data memory
        # return 32 bit hex val

        Instr_list = self.DMem[ReadAddress: ReadAddress+4]
        return ''.join(Instr_list)

    def writeDataMem(self, Address, WriteData: str):
        # write data into byte addressable memory
        # print("wirte data memory:\n Address: ", int(Address))

        self.DMem[Address] = WriteData[0:8]
        self.DMem[Address+1] = WriteData[8:16]
        self.DMem[Address+2] = WriteData[16:24]
        self.DMem[Address+3] = WriteData[24:32]

    def outputDataMem(self):
        resPath = self.ioDir + "\\" + self.id + "_DMEMResult.txt"
        with open(resPath, "w") as rp:
            rp.writelines([str(data) + "\n" for data in self.DMem])


class RegisterFile(object):
    def __init__(self, ioDir):
        self.outputFile = ioDir + "RFResult.txt"
        # self.Registers = [0x0 for i in range(32)]
        self.Registers = [
            '00000000000000000000000000000000' for i in range(32)]

    def readRF(self, Reg_addr: int) -> int:
        # Fill in
        if(Reg_addr < 0 or Reg_addr > 31):
            print("Out of bound: Reg_addr = ", Reg_addr)

        # print("readRF: Reg:", Reg_addr, "value: ", self.Registers[Reg_addr])
        if self.Registers[Reg_addr] == 0x0:
            value = int('00000000000000000000000000000000', 2)
        else:
            value = int(self.Registers[Reg_addr], 2)
        return value

    def writeRF(self, Reg_addr: int, Wrt_reg_data):
        # Fill in
        if(Reg_addr < 0 or Reg_addr > 31):
            print("Out of bound: Reg_addr = ", Reg_addr)

        # R0 is reserved to be ZERO
        if Reg_addr == 0:
            return

        # Wrt_reg_data could be str or int
        # convert to string of binary if needed
        if type(Wrt_reg_data) == str:
            self.Registers[Reg_addr] = Wrt_reg_data
        else:
            data_in_binary = '{:032b}'.format(Wrt_reg_data & 0xFFFFFFFF)
            self.Registers[Reg_addr] = data_in_binary

        # print("Reg", Reg_addr, ": type(", type(Wrt_reg_data), "):", Wrt_reg_data)
        # self.Registers[Reg_addr] = Wrt_reg_data

    def outputRF(self, cycle):
        # op = ["-"*70+"\n", "State of RF after executing cycle: " +
        #       str(cycle) + "\n"]
        op = ["State of RF after executing cycle:   " +
              str(cycle) + "\n"]
        op.extend([str(val)+"\n" for val in self.Registers])
        if(cycle == 0):
            perm = "w"
        else:
            perm = "a"
        with open(self.outputFile, perm) as file:
            file.writelines(op)


class State(object):
    def __init__(self):
        self.IF = {"nop": False, "PC": 0,
                   "PCsrc": 0,
                   "BranchAddress": 0}

        self.ID = {"nop": True, "Instr": 0,
                   "PC": 0,
                   "Wrt_Reg_En": 0,
                   "Wrt_Reg_Address": 0,
                   "Wrt_Reg_data": 0}

        self.EX = {"nop": True,
                   "Read_data1": 0, "Read_data2": 0,
                   "Imm": 0,
                   "Wrt_Reg_En": 0,
                   "Wrt_Reg_Address": 0,
                   "Mnemonic": 0}

        self.MEM = {"nop": True,
                    "ALUresult": 0,
                    "store_data": 0,
                    "Wrt_Reg_En": 0,
                    "Wrt_Reg_Address": 0,
                    "Mnemonic": 0}
        self.WB = {"nop": True,
                   "Mem_data": 0,
                   "ALUresult": 0,
                   "Wrt_Reg_Address": 0,
                   "Wrt_Reg_En": 0,
                   "Mnemonic": 0
                   }


class Core(object):
    def __init__(self, ioDir, imem, dmem):
        self.myRF = RegisterFile(ioDir)
        self.cycle = 0
        self.halted = False
        self.ioDir = ioDir
        self.state = State()
        self.nextState = State()
        self.ext_imem = imem
        self.ext_dmem = dmem


class SingleStageCore(Core):
    def __init__(self, ioDir, imem, dmem):
        super(SingleStageCore, self).__init__(ioDir + "\\SS_", imem, dmem)
        self.opFilePath = ioDir + "\\StateResult_SS.txt"

    def int_to_32bit_str(integer: int):
        return format(integer, '032b')

    def step(self):
        # Your implementation

        # IF

        #print("Cycle: ", self.cycle, " PC: ", self.state.IF["PC"])
        instr_str = self.ext_imem.readInstr(self.state.IF["PC"])

        # decode instruction

        # [0:7]   [7:12][12:17]
        #                     [-15:-12]
        #                         [-12:-7]
        #                               [-7:]
        # func7   rs2   rs1   fu3 rd    opcode
        # 31_25   24_20 19_15     11_7  6_0
        # 0000000 00000 00000 000 00000 0000000

        # rd = self.myRF.readRF(int(hexbit[-12:-7]))
        # rs2 = self.myRF.readRF(int(hexbit[-25:-20]))
        # rs1 = self.myRF.readRF(int(hexbit[-20:-15]))
        # funct3 = hexbit[-15:-12]

        opcode = instr_str[-7:]
        rd_or_imm = instr_str[-12:-7]
        funct3 = instr_str[-15:-12]
        rs1 = instr_str[12:17]
        rs2 = instr_str[7:12]
        funct7_or_imm = instr_str[0:7]

        increment_PC = 1
        # update the PC in the next state
        pc = self.state.IF["PC"]

        # ID Instruction Decode

        # check opcode
        # Type R instruction
        if opcode == '0110011':
            if funct3 == '000':
                if funct7_or_imm == '0000000':
                    mnemonic = 'ADD'
                else:
                    mnemonic = 'SUB'

            elif funct3 == '100':
                mnemonic = 'XOR'
            elif funct3 == '110':
                mnemonic = 'OR'
            else:
                mnemonic = 'AND'

        # Type I instruction
        elif opcode == '0010011':
            imm_I = instr_str[0:12]
            if funct3 == '000':
                mnemonic = 'ADDI'
            elif funct3 == '100':
                mnemonic = 'XORI'
            elif funct3 == '110':
                mnemonic = 'ORI'
            else:
                mnemonic = 'ANDI'

        # Type J, JUMP instruction
        elif opcode == '1101111':
            # [0:7]   [7:12][12:17]
            #                     [-15:-12]
            #                         [-12:-7]
            #                               [-7:]
            # func7   rs2   rs1   fu3 rd    opcode
            # 31_25   24_20 19_15     11_7  6_0
            # 0000000 00000 00000 000 00000 0000000
            imm_20 = instr_str[0]
            imm_10_1 = instr_str[1:11]
            imm_11 = instr_str[11]
            imm_19_12 = instr_str[12:20]
            imm_J = imm_20 + imm_19_12 + imm_11 + imm_10_1 + '0'
            mnemonic = 'JAL'

        # Type B
        elif opcode == '1100011':
            imm_B = instr_str[-32]+instr_str[-8] + \
                instr_str[-31:-25]+instr_str[-12:-8]+'0'
            if funct3 == '000':
                mnemonic = 'BEQ'
            else:
                mnemonic = 'BNE'

        # Load instruction
        elif opcode == '0000011':
            imm_L = instr_str[0:12]
            mnemonic = 'LW'

        # store instruction
        elif opcode == '0100011':
            imm_S = funct7_or_imm+rd_or_imm
            mnemonic = 'SW'
        else:
            mnemonic = 'HALT'

        print("mnemonic: ", mnemonic, '\n')

        # print("hexbit = ", hexbit)
        # print("flag = ", mnemonic)

        # Execute

        # Type R instructions
        if mnemonic == 'ADD':
            # rd = rs1 + rs2

            # print(self.myRF.readRF(int(rs1, 2)),
            #       "+", self.myRF.readRF(int(rs2, 2)))
            # print("=", self.myRF.readRF(int(rs1, 2))
            #       + self.myRF.readRF(int(rs2, 2)))

            self.myRF.writeRF(int(rd_or_imm, 2),
                              self.myRF.readRF(int(rs1, 2))
                              + self.myRF.readRF(int(rs2, 2)))

        elif mnemonic == 'SUB':
            self.myRF.writeRF(int(rd_or_imm, 2),
                              self.myRF.readRF(int(rs1, 2))
                              - self.myRF.readRF(int(rs2, 2)))

        elif mnemonic == 'XOR':
            self.myRF.writeRF(int(rd_or_imm, 2),
                              self.myRF.readRF(int(rs1, 2))
                              ^ self.myRF.readRF(int(rs2, 2)))

        elif mnemonic == 'OR':
            self.myRF.writeRF(int(rd_or_imm, 2),
                              self.myRF.readRF(int(rs1, 2))
                              | self.myRF.readRF(int(rs2, 2)))

        elif mnemonic == 'AND':
            self.myRF.writeRF(int(rd_or_imm, 2),
                              self.myRF.readRF(int(rs1, 2))
                              & self.myRF.readRF(int(rs2, 2)))

        # Type I instructions
        elif mnemonic == 'ADDI':
            Reg_data = self.myRF.readRF(int(rs1, 2))
            imm_num = twos_complement_to_int(imm_I)
            self.myRF.writeRF(int(rd_or_imm, 2), Reg_data + imm_num)

        elif mnemonic == 'XORI':
            Reg_data = self.myRF.readRF(int(rs1, 2))
            imm_num = twos_complement_to_int(imm_I)
            self.myRF.writeRF(int(rd_or_imm, 2), Reg_data ^ imm_num)

        elif mnemonic == 'ORI':
            Reg_data = self.myRF.readRF(int(rs1, 2))
            imm_num = twos_complement_to_int(imm_I)
            self.myRF.writeRF(int(rd_or_imm, 2), Reg_data | imm_num)

        elif mnemonic == 'ANDI':
            Reg_data = self.myRF.readRF(int(rs1, 2))
            imm_num = twos_complement_to_int(imm_I)
            self.myRF.writeRF(int(rd_or_imm, 2), Reg_data & imm_num)

        # Jump and Link instruction

        elif mnemonic == 'JAL':
            increment_PC = 0
            self.myRF.writeRF(int(rd_or_imm, 2), pc + 4)
            # update PC for next cycle
            relative_addressing = twos_complement_to_int(imm_J)
            self.state.IF["PC"] = self.state.IF["PC"] + \
                relative_addressing

        # type B
        elif mnemonic == 'BEQ':
            if self.myRF.readRF(int(rs1, 2)) == self.myRF.readRF(int(rs2, 2)):
                increment_PC = 0
                self.state.IF["PC"] = self.state.IF["PC"] + \
                    twos_complement_to_int(imm_B)
            else:
                pass

        elif mnemonic == 'BNE':
            if self.myRF.readRF(int(rs1, 2)) == self.myRF.readRF(int(rs2, 2)):
                pass
            else:
                increment_PC = 0
                self.state.IF["PC"] = self.state.IF["PC"] + \
                    twos_complement_to_int(imm_B)

        # load instruction
        elif mnemonic == 'LW':
            # get data from memory address[rs1 + imm]
            imm_num = twos_complement_to_int(imm_L)
            # print("data address to load: [", int(rs1, 2) + int(imm, 2), "]")
            data = self.ext_dmem.readInstr(int(rs1, 2) + imm_num)
            # print("data:", data)
            # print("load to Reg ", int(rd_or_imm, 2))
            self.myRF.writeRF(int(rd_or_imm, 2), data)

        # store instruction
        elif mnemonic == 'SW':

            # print("rs1: ", int(rs1, 2))
            # print("+ ", int(imm, 2))
            # print("store at Dmem[", int(rs1, 2) + int(imm, 2), "]")
            # print("data to store: ", self.myRF.readRF(int(rs2, 2)))

            imm_num = twos_complement_to_int(imm_S)
            int_data = self.myRF.readRF(int(rs2, 2))

            self.ext_dmem.writeDataMem(int(rs1, 2) + imm_num,
                                       bin(int_data)[2:].zfill(32))

        else:
            # HALT instruction

            pass

        # update PC
        if increment_PC == 1 and self.state.IF["nop"] == False and mnemonic != 'HALT':
            self.nextState.IF["PC"] = pc + 4

        if self.state.IF["nop"]:
            self.halted = True

        if mnemonic == 'HALT':
            self.nextState.IF["nop"] = True

        self.myRF.outputRF(self.cycle)  # dump RF
        # print states after executing cycle 0, cycle 1, cycle 2 ...
        self.printState(self.nextState, self.cycle)

        # The end of the cycle and updates the current state with the values calculated in this cycle
        self.state = self.nextState
        self.cycle += 1

    def printState(self, state, cycle):
        printstate = [
            # "-"*70+"\n",
            "State after executing cycle: " + str(cycle) + "\n"]
        printstate.append("IF.PC: " + str(state.IF["PC"]) + "\n")
        printstate.append("IF.nop: " + boolean_value(state.IF["nop"]) + "\n")

        if(cycle == 0):
            perm = "w"
        else:
            perm = "a"
        with open(self.opFilePath, perm) as wf:
            wf.writelines(printstate)


class FiveStageCore(Core):
    def __init__(self, ioDir, imem, dmem):
        super(FiveStageCore, self).__init__(ioDir + "\\FS_", imem, dmem)
        self.opFilePath = ioDir + "\\StateResult_FS.txt"

    def step(self):
        # Your implementation
        # --------------------- WB stage ---------------------

        if not self.state.WB["nop"]:
            mnemonic = self.state.WB["Mnemonic"]

            if mnemonic == 'LW':
                self.nextState.ID["Wrt_Reg_data"] = self.state.WB["Mem_data"]

            else:
                self.nextState.ID["Wrt_Reg_data"] = self.state.WB["ALUresult"]

            # pass attributes
            self.nextState.ID["Wrt_Reg_En"] = self.state.WB["Wrt_Reg_En"]
            self.nextState.ID["Wrt_Reg_Address"] = self.state.WB["Wrt_Reg_Address"]

        # --------------------- MEM stage --------------------

        if not self.state.MEM["nop"]:
            mnemonic = self.state.MEM["Mnemonic"]

            if mnemonic == 'LW':
                self.nextState.WB["Mem_data"] = self.ext_dmem.readInstr(
                    self.state.MEM["ALUresult"])

            if mnemonic == 'SW':
                self.ext_dmem.writeDataMem(self.state.MEM["ALUresult"],
                                           bin(self.state.MEM["store_data"])[2:].zfill(32))

                # pass attributes
            self.nextState.WB["Wrt_Reg_En"] = self.state.MEM["Wrt_Reg_En"]
            self.nextState.WB["Wrt_Reg_Address"] = self.state.MEM["Wrt_Reg_Address"]
            self.nextState.WB["ALUresult"] = self.state.MEM["ALUresult"]
            self.nextState.WB["Mnemonic"] = self.state.MEM["Mnemonic"]
            self.nextState.WB["nop"] = False
        else:
            self.nextState.WB["nop"] = True

        # --------------------- EX stage ---------------------

        if not self.state.EX["nop"]:
            mnemonic = self.state.EX["Mnemonic"]
            ALU_result = 'NULL'

            # # generate ALU_result for next EX stage
            if mnemonic in ALU_type_R:
                if mnemonic == 'ADD':
                    ALU_result = self.state.EX["Read_data1"] + \
                        self.state.EX["Read_data2"]
                elif mnemonic == 'SUB':
                    ALU_result = self.state.EX["Read_data1"] - \
                        self.state.EX["Read_data2"]
                elif mnemonic == 'OR':
                    ALU_result = self.state.EX["Read_data1"] | \
                        self.state.EX["Read_data2"]
                elif mnemonic == 'XOR':
                    ALU_result = self.state.EX["Read_data1"] ^ \
                        self.state.EX["Read_data2"]
                else:
                    ALU_result = self.state.EX["Read_data1"] & \
                        self.state.EX["Read_data2"]

            elif mnemonic in ALU_type_I_L_S:
                if mnemonic == 'ADDI':
                    ALU_result = self.state.EX["Read_data1"] + \
                        twos_complement_to_int(self.state.EX["Imm"])
                elif mnemonic == 'ORI':
                    ALU_result = self.state.EX["Read_data1"] | \
                        twos_complement_to_int(self.state.EX["Imm"])
                elif mnemonic == 'XORI':
                    ALU_result = self.state.EX["Read_data1"] ^ \
                        twos_complement_to_int(self.state.EX["Imm"])
                elif mnemonic == 'ANDI':
                    ALU_result = self.state.EX["Read_data1"] & \
                        twos_complement_to_int(self.state.EX["Imm"])
                # if LW or SW
                else:
                    ALU_result = self.state.EX["Read_data1"] + \
                        twos_complement_to_int(self.state.EX["Imm"])

            # generate attributes for next MEM stage
            # if ALU_result == 'NULL':
            #     print("ALU result undefined")
            self.nextState.MEM["ALUresult"] = ALU_result
            self.nextState.MEM["store_data"] = self.state.EX["Read_data2"]
            self.nextState.MEM["Wrt_Reg_Address"] = self.state.EX["Wrt_Reg_Address"]
            self.nextState.MEM["Wrt_Reg_En"] = self.state.EX["Wrt_Reg_En"]
            self.nextState.MEM["Mnemonic"] = mnemonic

            self.nextState.MEM["nop"] = False

        else:
            self.nextState.MEM["nop"] = True
            # --------------------- ID stage ---------------------

            # self.ID = {"nop": False, "Instr": 0}

        if not self.state.ID["nop"]:

            # # pass on PC
            # self.nextState.EX["PC"] = self.state.ID["PC"]

            # decode the instruction and get mnemonic and imm
            instr_str = self.state.ID["Instr"]

            opcode = instr_str[-7:]
            rd_or_imm = instr_str[-12:-7]
            funct3 = instr_str[-15:-12]
            rs1 = instr_str[12:17]
            rs2 = instr_str[7:12]
            funct7_or_imm = instr_str[0:7]

            # read register
            read_data1 = self.myRF.readRF(int(rs1, 2))
            read_data2 = self.myRF.readRF(int(rs2, 2))

            # check opcode
            # type R
            if opcode == '0110011':

                # imm is irrelavent
                imm = 0xFFFFFFFF
                if funct3 == '000':
                    if funct7_or_imm == '0000000':
                        mnemonic = 'ADD'
                    else:
                        mnemonic = 'SUB'

                elif funct3 == '100':
                    mnemonic = 'XOR'
                elif funct3 == '110':
                    mnemonic = 'OR'
                else:
                    mnemonic = 'AND'

            # Type I instruction
            elif opcode == '0010011':
                imm = instr_str[0:12]
                if funct3 == '000':
                    mnemonic = 'ADDI'
                elif funct3 == '100':
                    mnemonic = 'XORI'
                elif funct3 == '110':
                    mnemonic = 'ORI'
                else:
                    mnemonic = 'ANDI'

            # Type J, JUMP instruction
            elif opcode == '1101111':
                # [0:7]   [7:12][12:17]
                #                     [-15:-12]
                #                         [-12:-7]
                #                               [-7:]
                # func7   rs2   rs1   fu3 rd    opcode
                # 31_25   24_20 19_15     11_7  6_0
                # 0000000 00000 00000 000 00000 0000000
                imm_20 = instr_str[0]
                imm_10_1 = instr_str[1:11]
                imm_11 = instr_str[11]
                imm_19_12 = instr_str[12:20]
                imm = imm_20 + imm_19_12 + imm_11 + imm_10_1 + '0'
                mnemonic = 'JAL'

            # Type B
            elif opcode == '1100011':
                imm = instr_str[-32]+instr_str[-8] + \
                    instr_str[-31:-25]+instr_str[-12:-8]+'0'
                if funct3 == '000':
                    mnemonic = 'BEQ'
                else:
                    mnemonic = 'BNE'

            # Load instruction
            elif opcode == '0000011':
                imm = instr_str[0:12]
                mnemonic = 'LW'

            # store instruction
            elif opcode == '0100011':
                imm = funct7_or_imm+rd_or_imm
                mnemonic = 'SW'
            else:
                mnemonic = 'HALT'
                imm = 0xFFFFFFFF

            #print("mnemonic: ", mnemonic, '\n', "imm: ", imm)

            # check if the instruction will branch or not
            if mnemonic == 'BNE' or mnemonic == 'BEQ' or mnemonic == 'JAL':
                if mnemonic == 'JAL':
                    # generate branch address and take branch on the next IF
                    self.nextState.IF["BranchAddress"] = self.state.ID["PC"] + \
                        twos_complement_to_int(imm)
                    self.nextState.IF["PCsrc"] = 1
                    # nop next ID
                    self.nextState.ID["nop"] = True

                    # store pc + 4 into rd
                    self.myRF.writeRF(int(rd_or_imm, 2),
                                      self.state.ID["PC"] + 4)

                if mnemonic == 'BEQ' and read_data1 == read_data2:
                    self.nextState.IF["BranchAddress"] = self.state.ID["PC"] + \
                        twos_complement_to_int(imm)
                    self.nextState.IF["PCsrc"] = 1
                    # nop next ID
                    self.nextState.ID["nop"] = True

                if mnemonic == 'BNE' and read_data1 != read_data2:
                    self.nextState.IF["BranchAddress"] = self.state.ID["PC"] + \
                        twos_complement_to_int(imm)
                    self.nextState.IF["PCsrc"] = 1
                    # nop next ID
                    self.nextState.ID["nop"] = True

            # write register
            if self.state.ID["Wrt_Reg_En"]:
                self.myRF.writeRF(
                    int(self.state.ID["Wrt_Reg_Address"], 2), self.state.ID["Wrt_Reg_data"])

            # if mnemonic is HALT, deactivate next IF
            if mnemonic == 'HALT':
                self.nextState.IF["nop"] = True

            # generate attributes for next EX stage

            # generate Read_data1 and 2 for next EX
            self.nextState.EX["Read_data1"] = read_data1
            self.nextState.EX["Read_data2"] = read_data2

            # generate mnemonic for next EX
            self.nextState.EX["Mnemonic"] = mnemonic

            # generate imm for nest EX stage
            self.nextState.EX["Imm"] = imm

            # generate Wrt_Reg_address for next EX stage
            self.nextState.EX["Wrt_Reg_address"] = rd_or_imm

            self.nextState.EX["nop"] = False

        else:
            self.nextState.EX["nop"] = True

        # --------------------- IF stage ---------------------

        # need PCsrc to determine branch or not
        # need branch address

        # if current IF stage is not NOP, active
        if not self.state.IF["nop"]:

            # self.count = self.count + 1

            # generate next ID stage attributes: PC , Instr

            # if PCscr = 1 , branch.
            if self.state.IF["PCsrc"] == 1:

                # generate PC for next ID stage
                self.nextState.ID["PC"] = self.state.IF["BranchAddress"]
                # Instr for next ID is at branch address
                self.nextState.ID["Instr"] = self.ext_imem.readInstr(
                    self.state.IF["BranchAddress"])

            # if not, PC for ID is PC,
            # Instr for ID is at PC
            else:
                self.nextState.ID["PC"] = self.state.IF["PC"]
                self.nextState.ID["Instr"] = self.ext_imem.readInstr(
                    self.state.IF["PC"])

            # generate next IF stage attribute: PC
            self.nextState.IF["PC"] = self.state.IF["PC"] + 4

            # activate next ID
            self.nextState.ID["nop"] = False
        else:
            self.nextState.ID["nop"] = True
        # ---------------------- END ---------------------------
        if self.state.IF["nop"] and self.state.ID["nop"] and self.state.EX["nop"] and self.state.MEM["nop"] and self.state.WB["nop"]:
            self.halted = True

        self.myRF.outputRF(self.cycle)  # dump RF
        # print states after executing cycle 0, cycle 1, cycle 2 ...
        self.printState(self.nextState, self.cycle)

        # The end of the cycle and updates the current state with the values calculated in this cycle
        self.state = self.nextState
        self.cycle += 1

    def printState(self, state, cycle):
        printstate = ["-"*70+"\n",
                      "State after executing cycle: " + str(cycle) + "\n"]
        printstate.extend(["IF." + key + ": " + str(val) +
                          "\n" for key, val in state.IF.items()])
        printstate.extend(["ID." + key + ": " + str(val) +
                          "\n" for key, val in state.ID.items()])
        printstate.extend(["EX." + key + ": " + str(val) +
                          "\n" for key, val in state.EX.items()])
        printstate.extend(["MEM." + key + ": " + str(val) +
                          "\n" for key, val in state.MEM.items()])
        printstate.extend(["WB." + key + ": " + str(val) +
                          "\n" for key, val in state.WB.items()])

        if(cycle == 0):
            perm = "w"
        else:
            perm = "a"
        with open(self.opFilePath, perm) as wf:
            wf.writelines(printstate)


if __name__ == "__main__":

    # parse arguments for input file location
    parser = argparse.ArgumentParser(description='RV32I processor')
    parser.add_argument('--iodir', default="", type=str,
                        help='Directory containing the input files.')
    args = parser.parse_args()

    ioDir = os.path.abspath(args.iodir)
    print("IO Directory:", ioDir)

    imem = InsMem("Imem", ioDir)
    dmem_ss = DataMem("SS", ioDir)
    dmem_fs = DataMem("FS", ioDir)

    ssCore = SingleStageCore(ioDir, imem, dmem_ss)
    fsCore = FiveStageCore(ioDir, imem, dmem_fs)

    while(True):
        if not ssCore.halted:
            ssCore.step()

        if not fsCore.halted:
            fsCore.step()

        if ssCore.halted and fsCore.halted:
            break

    # dump SS and FS data mem.
    dmem_ss.outputDataMem()
    dmem_fs.outputDataMem()
