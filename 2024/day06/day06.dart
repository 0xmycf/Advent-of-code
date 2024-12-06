/*
    Sovling Day 6 of AoC 2024
*/

import 'dart:io';

Future<void> main(List<String> arguments) async {
  var map = File("../input/day06.txt");
  // var map = File("./test.txt");
  var lines = await map.readAsLines();
  var board = Board.fromString(lines);

  print(board.withOtherBuffer(() => board.partOne(true)));
  // print(board.withOtherBuffer(() => board.partOne()));
}

class Pos {
  final int x;
  final int y;

  Pos(this.x, this.y);

  @override
  bool operator ==(Object other) {
    if (other is Pos) {
      return other.x == x && other.y == y;
    }
    return false;
  }

  @override
  int get hashCode => Object.hash(x, y);

  Pos move(Dir dir) {
    switch (dir) {
      case Dir.north:
        return Pos(x - 1, y); // uh we are in Austrailia ig
      case Dir.south:
        return Pos(x + 1, y);
      case Dir.east:
        return Pos(x, y + 1);
      case Dir.west:
        return Pos(x, y - 1);
    }
  }

  isOn(Board board) {
    return x >= 1 && y >= 1 && x <= board.height && y <= board.width;
  }
}

enum Dir { north, south, west, east }

class Guard {
  Pos pos;
  Dir dir;
  Set<Pos> visited;

  Guard(this.pos, this.dir, this.visited);
  Guard.fromInts(int x, int y) : this(Pos(x, y), Dir.north, {Pos(x, y)});
  @override
  String toString() {
    String ret = "";
    switch (dir) {
      case Dir.north:
        ret = '^';
        break;
      case Dir.south:
        ret = 'v';
        break;
      case Dir.east:
        ret = '>';
        break;
      case Dir.west:
        ret = '<';
        break;
    }
    return ret;
  }

  void move([Dir? argdir]) {
    if (argdir != null) {
      pos = pos.move(argdir);
    } else {
      pos = pos.move(dir);
    }
    visited.add(pos);
  }

  /// Rotates 90 degrees to the right
  void rotateRight() {
    switch (dir) {
      case Dir.north:
        dir = Dir.east;
        break;
      case Dir.east:
        dir = Dir.south;
        break;
      case Dir.south:
        dir = Dir.west;
        break;
      case Dir.west:
        dir = Dir.north;
        break;
    }
  }

  bool isOn(Board board) {
    return pos.isOn(board);
  }

  Pos peek() {
    return pos.move(dir);
  }
}

class Board {
  int width;
  int height;
  Guard guard;
  List<Pos> obstacles;
  Map<Pos, bool> obstaclesMap;

  factory Board._internal(
      int width, int height, List<Pos> positions, Guard guard) {
    return Board(width, height, positions,
        {for (var elem in positions) elem: true}, guard);
  }

  Board(this.width, this.height, this.obstacles, this.obstaclesMap, this.guard);

  factory Board.fromString(List<String> lines) {
    List<Pos> positions = [];
    int width = 0;
    int height = 0;
    late Guard guard;
    for (var line in lines) {
      height++;
      width = 0;
      for (var c in line.split('')) {
        width++;
        if (c == '#') {
          positions.add(Pos(height, width));
        } else if (c == '^') {
          guard = Guard.fromInts(height, width);
        }
      }
    }
    return Board._internal(width, height, positions, guard);
  }

  T withOtherBuffer<T>(T Function() fn) {
    // enable alterantive buffer
    stdout.write("\x1b 7"); // save cursor
    stdout.write("\x1b[?25l"); // makes cursor invisible
    stdout.write("\x1b[?1049h");
    stdout.write("\x1b[H"); // move cursor to home position
    var ret = fn();
    stdout.write("\x1b[?1049l");
    stdout.write("\x1b[?25h"); // makes cursor visible again
    stdout.write("\x1b 8"); // restore cursor
    stdout.write("\x1b[J");
    return ret;
  }

  /// renders the map on the console
  void render() {
    visit((pos) {
      if (obstaclesMap.containsKey(pos)) {
        stdout.write('#');
      } else if (guard.pos == pos) {
        stdout.write(guard.toString());
      } else {
        stdout.write(".");
      }
    }, (x) {
      if (x != height) {
        stdout.write("\n");
      }
    });
  }

  String getField(Pos pos) {
    if (obstaclesMap.containsKey(pos)) {
      return "#";
    } else if (guard.pos == pos) {
      return guard.toString();
    } else if (guard.visited.contains(pos)) {
      return "\x1b[31mX\x1b[0m";
    } else {
      return ".";
    }
  }

  void reRender(Pos prev, Pos curr) {
    moveCursor(line, column) => stdout.write("\x1b[${line + 1};${column}H");

    for (var pos in [prev, curr]) {
      moveCursor(pos.x, pos.y);
      stdout.write(getField(pos));
    }
    // sleep(Duration(milliseconds: 150));
    sleep(Duration(milliseconds: 80));
  }

  int partOne([bool withGraphics = false]) {
    if (withGraphics) {
      render();
    }
    Pos prev = guard.pos;
    while (guard.isOn(this)) {
      prev = guard.pos;
      Pos next = guard.peek();
      if (obstaclesMap.containsKey(next)) {
        guard.rotateRight();
      }
      guard.move();
      if (withGraphics) {
        reRender(prev, guard.pos);
      }
    }
    return guard.visited.where((pos) => pos.isOn(this)).length;
  }

  /// visit each node and apply fn on the current position
  void visit(void Function(Pos) fn, void Function(int) outerLoop) {
    for (var x = 1; x <= height; x++) {
      for (var y = 1; y <= width; y++) {
        fn(Pos(x, y));
      }
      outerLoop(x);
    }
  }

  @override
  String toString() {
    final buf = StringBuffer();
    for (var x = 1; x <= height; x++) {
      for (var y = 1; y <= width; y++) {
        var curr = Pos(x, y);
        if (guard.pos == curr) {
          buf.write(guard.toString());
          continue;
        }
        if (obstaclesMap.containsKey(curr)) {
          buf.write("#");
        } else {
          buf.write(".");
        }
      }
      if (x != height) {
        buf.writeln();
      }
    }
    return buf.toString();
  }
}
