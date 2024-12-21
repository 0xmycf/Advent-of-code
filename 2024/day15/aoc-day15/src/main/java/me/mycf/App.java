package me.mycf;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;

/**
 * Exit code - 0 fine - 1 some error
 */
public class App {
    public static void main(String[] args) throws FileNotFoundException {
        ParseResult result = parse("../../input/day15.txt");
        // ParseResult result = parse("../test.txt");
        Grid grid = Grid.fromString(result.map());

        for (char c : result.instructions().toCharArray()) {
            grid.movePlayer(Instruction.from(c));
        }
        System.out.println(grid.prettyPrint());
        System.out.println(grid.getScore());
    }

    private record ParseResult(String map, String instructions) {
    };

    private static ParseResult parse(String path) throws FileNotFoundException {

        String map;
        String instrs;
        {
            File input = new File(path);
            InputStreamReader reader = new InputStreamReader(new FileInputStream(input));
            StringBuilder mapb = new StringBuilder();
            StringBuilder instrsb = new StringBuilder();
            boolean fh = true; /* whether we're in the first half of the input or not */
            try (BufferedReader br = new BufferedReader(reader)) {
                String line;
                while ((line = br.readLine()) != null) {
                    if (line.equals("")) {
                        fh = false;
                        continue;
                    }
                    if (fh) {
                        mapb.append(line + "\n");
                    } else {
                        instrsb.append(line);
                    }
                }

            } catch (IOException iox) {
                System.out.println("IOX happend, sorry for that...");
                System.exit(1);
            }
            map = mapb.toString();
            instrs = instrsb.toString();

        }

        return new ParseResult(map, instrs);
    }

    /* @formatter:off
        String s = ("""
                ##############
                # O      #   #
                #     #      #
                #  @     O   #
                #     #  O   #
                # O      #   #
                #            #
                ##############
                """);
        System.out.println("Orig: ");
        System.out.println(s);
        System.out.println();
        var grid = Grid.fromString(s);
        System.out.println(grid.prettyPrint());

        try (Scanner input = new Scanner(System.in)) {
            while (true) {
                var str = input.next();
                grid.movePlayer(Instruction.from(str));
                System.out.println(grid.prettyPrint());
            }
        }
        @formatter:off
    */
}
