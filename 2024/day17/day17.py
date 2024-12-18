#! /usr/bin/env python3

from abc import ABC
from typing import Callable, Optional
from pathlib import Path
from os.path import isfile

class Register():# {{{
    name: str
    _value: int
    _history: list[int]

    def __init__(self, name, initial_value) -> None:
        self.name = name
        self._value = initial_value
        self._history = []

    def put(self, new_value) -> None:
        self._value = new_value
        self._history.append(new_value)

    def swap(self, new_value) -> int:
        """
        Returns the previously stored value
        """
        tmp = self._value
        self._value = int(new_value)
        self._history.append(new_value) 
        return tmp

    def modify(self, fn: Callable[[int], int]) -> None:
        self._value = int(fn(self._value))
        self._history.append(self._value)
    
    def get(self) -> int:
       return self._value
    
    def debug_info(self) -> tuple[list[int], int]:
        return self._history, self._value

# }}}

Reg = dict[str, Register]

class Operant(ABC):# {{{
    def __init__(self, i, reg: Reg) -> None:
        self.i = i
        self.reg = reg
    def get(self) -> int: raise NotImplementedError()# }}}

# where to store and what to store
Instruction = Callable[[Reg, Operant], Optional[int]]

class LiteralOperant(Operant):# {{{
    def __init__(self, i) -> None:
        super().__init__(i, {})
    def get(self) -> int:
        return self.i# }}}

class ComboOperant(Operant):# {{{
    def get(self):
        match self.i:
            case 0 | 1 | 2 | 3:
                return self.i
            case 4:
                return self.reg["A"].get()
            case 5:
                return self.reg["B"].get()
            case 6:
                return self.reg["C"].get()
            case 7:
                raise Exception("7 is reserved and should never appear as an operant") 
        raise Exception(f"Illegal argument {self.i}, expected number lke [0-7]")# }}}

class Opcode:# {{{
    ADV = 0
    BXL = 1
    BST = 2
    JNZ = 3
    BXC = 4
    OUT = 5
    BDV = 6
    CDV = 7# }}}

INSTRUCTIONS: dict[int, Instruction] = {
        # numerator / denominator
        0: lambda reg, y: reg["A"].modify(lambda x: x / (2**y.get())),
        1: lambda reg,y: reg["B"].modify(lambda x: x ^ y.get()),
        2: lambda reg,y: reg["B"].put(y.get() % 8),
        3: lambda _reg,_y: None,
        4: lambda reg,_y: reg["B"].modify(lambda b: b ^ reg["C"].get()),
        5: lambda _reg,y: y.get() % 8,
        6: lambda reg,y: reg["B"].put(int(reg["A"].get() / (2**y.get()))),
        7: lambda reg,y: reg["C"].put(int(reg["A"].get() / (2**y.get()))),
}

class OperantFactory():
    @staticmethod
    def new_operant(opcode: int, operant: int, reg: Reg) -> Operant:
        match opcode:
            case 0:
                return ComboOperant(operant, reg)
            case 1:
                return LiteralOperant(operant)
            case 2:
                return ComboOperant(operant, reg)
            case 3:
                return Operant(-1, {}) # jnz is ignored
            case 4:
                return Operant(-1, {})
            case 5:
                return ComboOperant(operant, reg)
            case 6:
                return ComboOperant(operant, reg)
            case 7:
                return ComboOperant(operant, reg)
        raise Exception(f"opcode {opcode} out of range [0-7] for operant factory")


class Interpreter():
    # the increment after each step
    INC: int = 2
    instr_ptr: int
    operations: dict[int, Instruction]
    reg: dict[str, Register]

    def __init__(self, ops: dict[int, Instruction], registers: list[Register], num_regs = 3) -> None:
        self.operations = ops
        self.instr_ptr = 0
        self.reg = {}
        for r in registers:
            self.reg[r.name] = r
        if num_regs > len(registers):
            i = 0
            abc = "ABCDEFGHIJKLMNOPQRSTUVW"
            while i < num_regs:
                if self.reg.get(abc[i], None) is None:
                    self.reg[abc[i]] = Register(name=abc[i], initial_value=0)
                i += 1

    def run(self, program: list[int]) -> str:
        """
        returns the string representation we need for the problem
        (meaning 'out' instructions)
        """
        ret = []
        while self.instr_ptr <= len(program) - 2:
            print(f"[LOGGING]: Program is at {self.instr_ptr} ({program[self.instr_ptr]})")
            opt_s = self.read(program[self.instr_ptr], program[self.instr_ptr+1])
            if opt_s is not None:
                ret += opt_s
            if self.reg["A"].get() != 0 and program[self.instr_ptr] == Opcode.JNZ:
                self.instr_ptr = program[self.instr_ptr+1]
            else:
                self.instr_ptr += self.INC
        return ','.join(ret)

    def read(self, opcode: int, operant: int) -> Optional[str]:
        """
        Reads the next char / number and advances the state machine
        """
        if opcode == Opcode.JNZ:
            return None
        op = OperantFactory.new_operant(opcode, operant, self.reg)
        instr = self.operations[opcode]
        ret = instr(self.reg, op)
        if ret is not None:
            return str(ret)

    def debug_info(self) -> list[str]:
        return [f"Debug_info for register {s.name}: {s.debug_info()}" for s in self.reg.values()]

def parse_program(input: str) -> list[int]:
    foo = input.lstrip("Program: ").split(",")
    return [int(c) for c in foo]

def parse_registers(input: list[str]) -> list[Register]:
    regs = []
    for line in input:
        raw = line.lstrip("Register ")
        c, raw = raw[0], raw[1:].lstrip(": ")
        regs.append(Register(name = c, initial_value=int(raw))) 
    return regs

def read_input(path: Path) -> list[str]:
    if not isfile(path):
        raise Exception(f"Path {path} is not a file")
    value = ""
    with open(path, "r") as io:
        value = io.readlines()
    return [v for v in value if v != ""]

def main():
    input_path = Path("../input/day17.txt")
    input = read_input(input_path)
    # input = [
    # "Register A: 729",
    # "Register B: 0",
    # "Register C: 0",
    # "Program: 0,1,5,4,3,0" ]

    
    # ✅ If register C contains 9, the program 2,6 would set register B to 1.
    # ✅ If register A contains 10, the program 5,0,5,1,5,4 would output 0,1,2.
    # ✅ If register A contains 2024, the program 0,1,5,4,3,0 would output 4,2,5,6,7,7,7,7,3,1,0 and leave 0 in register A.
    # ✅ If register B contains 29, the program 1,7 would set register B to 26.
    # ✅ If register B contains 2024 and register C contains 43690, the program 4,0 would set register B to 44354.

    # input = [
    #         "Register B: 2024",
    #         "Register C: 43690",
    #         "Program: 4,0"
    #         ]

    regs = parse_registers([l for l in input if l.startswith("Register")])
    print(f"[LOGGING]: registers are {[(r.name, r.get()) for r in regs]}")
    program = parse_program(input[-1])
    print(f"[LOGGING]: program is: {program}")
    interpreter = Interpreter(INSTRUCTIONS, regs)
    result = interpreter.run(program)
    print(f"\n{result}")
    pprint_info = '\n'.join(interpreter.debug_info())
    print(f"\n\ndebug status\n{pprint_info}")

if __name__ == '__main__':
    main()
