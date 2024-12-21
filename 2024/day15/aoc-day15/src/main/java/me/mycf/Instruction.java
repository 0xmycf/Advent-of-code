package me.mycf;

enum Instruction {
    Up, Down, Left, Right;

    public static Instruction from(char c) {
        return switch (c) {
            case '<' -> Left;
            case '^' -> Up;
            case '>' -> Right;
            case 'v' -> Down;
            default -> throw new IllegalArgumentException("Char was not one of <>^v");
        };
    }

    public static Instruction from(String c) {
        return Instruction.from(c.charAt(0));
    }
}
