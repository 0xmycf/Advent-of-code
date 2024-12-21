package me.mycf;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

class Grid {
    final private Kind[][] grid;
    Pos playerPos;
    final private Pos bounds;

    final private Set<Pos> history = new HashSet<>();

    Grid(int maxx, int maxy, Iterable<Pos> boxes, Iterable<Pos> obstacles, Pos player) {
        this.grid = new Kind[maxx][maxy];
        this.playerPos = player;
        this.bounds = new Pos(maxx, maxy);
        Set<Pos> hs = new HashSet<>();
        for (var pos : boxes) {
            this.grid[pos.x][pos.y] = Kind.Box;
            hs.add(pos);
        }
        for (var pos : obstacles) {
            this.grid[pos.x][pos.y] = Kind.Bedrock;
            hs.add(pos);
        }
        for (int x = 0; x < this.bounds.x; x++) {
            for (int y = 0; y < this.bounds.y; y++) {
                if (!hs.contains(new Pos(x, y))) {
                    this.grid[x][y] = Kind.Free;
                }
            }
        }

    }

    public int getScore() {
        int sum = 0;
        for (int x = 0; x < this.bounds.x; x++) {
            for (int y = 0; y < this.bounds.y; y++) {
                Pos pos = new Pos(x, y);
                if (this.getCell(pos).equals(Kind.Box)) {
                    sum += pos.gpsCoordinate();
                }
            }
        }
        return sum;
    }

    public void movePlayer(Instruction istr) {
        final var newPos = playerPos.move(istr);
        if (grid[newPos.x][newPos.y].equals(Kind.Free)) {
            this.playerPos = newPos;
            return;
        }
        if (grid[newPos.x][newPos.y].equals(Kind.Bedrock)) {
            /* do nothing */
            return;
        }
        if (grid[newPos.x][newPos.y].equals(Kind.Box)) {
            boolean fine = false;

            Deque<Pos> movees = new ArrayDeque<>();
            for (var pos : newPos.raycast(istr, this.bounds)) {
                var kind = this.getCell(pos);
                if (kind.equals(Kind.Free)) {
                    fine = true;
                    break;
                }
                if (kind.equals(Kind.Box)) {
                    movees.addFirst(pos);
                    continue;
                }
                if (kind.equals(Kind.Bedrock)) {
                    break;
                }
            }

            if (fine) {
                // going through them in reverse order due to .addFirst call
                // earlier
                for (Pos move : movees) {
                    this.setCell(Kind.Free, move);
                    this.setCell(Kind.Box, move.move(istr));
                }
                this.playerPos = newPos;
            }
        }
        this.history.add(this.playerPos);
    }

    Kind getCell(Pos pos) {
        return this.grid[pos.x][pos.y];
    }

    void setCell(Kind kind, Pos pos) {
        this.grid[pos.x][pos.y] = kind;
    }

    public static enum Kind {
        Box, Free, Bedrock /* Minecraft reference */;
    }

    public static record Pos(int x, int y) {

        Pos move(Instruction instr) {
            return switch (instr) {
                case Up -> new Pos(this.x, this.y - 1);
                case Down -> new Pos(this.x, this.y + 1);
                case Left -> new Pos(this.x - 1, this.y);
                case Right -> new Pos(this.x + 1, this.y);
            };
        }

        Iterable<Pos> raycast(Instruction instr, Pos bounds) {
            var curr = this;
            var ret = new ArrayList<Pos>();
            while (curr.x < bounds.x && curr.x >= 0 && curr.y < bounds.y && curr.y >= 0) {
                ret.add(curr);
                curr = curr.move(instr);
            }
            return ret;
        }

        /*
         * The GPS coordinate of a box is equal to 100 times its distance from the top
         * edge of the map plus its distance from the left edge of the map.
         */
        public int gpsCoordinate() {
            return 100 * this.y + this.x;
        }
    }

    /**
     * Parses the grid from a string
     */
    static Grid fromString(String input) {
        int height = 0;
        List<Pos> boxes = new ArrayList<>();
        List<Pos> obstacles = new ArrayList<>();
        Pos player = null;
        var width = 0;
        var maxwidth = width;
        for (var c : input.toCharArray()) {
            if (c == '\n') {
                height++;
                if (width > maxwidth) {
                    maxwidth = width;
                }
                width = 0;
                continue;
            }
            switch (c) {
                case '#':
                    obstacles.add(new Pos(width, height));
                    break;
                case '.':
                    break;
                case 'O':
                    boxes.add(new Pos(width, height));
                    break;
                case '@':
                    player = new Pos(width, height);
                    break;
            }
            width++;
        }
        return new Grid(maxwidth, height, boxes, obstacles, player);
    }

    public String prettyPrint() {
        final var sb = new StringBuilder();
        for (int y = 0; y < this.bounds.y; y++) {
            for (int x = 0; x < this.bounds.x; x++) {
                Kind value = this.grid[x][y];

                if (playerPos.equals(new Pos(x, y))) {
                    sb.append('@');
                    continue;
                }

                switch (value) {
                    case Free:
                        sb.append('.');
                        break;
                    case Box:
                        sb.append('O');
                        break;
                    case Bedrock:
                        sb.append('#');
                        break;
                }
            }
            sb.append('\n');
        }

        return sb.toString();
    }

}
