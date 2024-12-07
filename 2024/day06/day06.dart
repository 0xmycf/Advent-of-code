/*
    Sovling Day 6 of AoC 2024
*/

import 'dart:io';

Future<void> main(List<String> arguments) async {
  // var map = File("../input/day06.txt");
  var map = File("./test.txt");
  var lines = await map.readAsLines();
  var board = Board.fromString(lines, LabelOverseer.get());
  board.partOne();

  // Renderable.withOtherBuffer(() {
  //   // board.render();
  //   var seg = Segement(from: Pos(1, 1), to: Pos(1, 11), direction: Dir.north);
  //   var seg2 = Segement(from: Pos(1, 1), to: Pos(11, 1), direction: Dir.east);
  //   var seg3 =
  //       Segement(from: Pos(11, 11), to: Pos(11, 1), direction: Dir.south);
  //   var seg4 = Segement(from: Pos(11, 11), to: Pos(1, 11), direction: Dir.west);
  //   for (var s in [seg, seg2, seg3, seg4]) {
  //     s.render();
  //   }
  //   Pos(11, 11).render();
  //   Pos(1, 1).render();
  //   Pos(1, 11).render();
  //   Pos(11, 1).render();
  // });
}

class LabelOverseer {
  Label _last = Label(0);
  // ignore: non_constant_identifier_names
  static final LabelOverseer INSTANCE = LabelOverseer();
  Label next() {
    var label = Label(_last.id);
    _last = label;
    return label;
  }

  static LabelOverseer get() {
    return INSTANCE;
  }
}

class Label {
  final int id;
  Label(this.id);
}

void moveCursor(int line, int column) {
  // idk why line must be modified
  // stdout.write("\x1b[${line + 1};${column}H");
  stdout.write("\x1b[$line;${column}H");
}

abstract interface class Renderable {
  void render();
  static final String RED_X = "\x1b[31mX\x1b[0m";

  static T withOtherBuffer<T>(T Function() fn) {
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
}

class Segement implements Renderable {
  final Pos from;
  final Pos to;
  final Dir direction;

  Segement({required this.from, required this.to, required this.direction})
      : assert(switch (direction) {
          Dir.north => from.x == to.x && from.y <= to.y,
          Dir.south => from.x == to.x && from.y >= to.y,
          Dir.west => from.y == to.y && from.x >= to.x,
          Dir.east => from.y == to.y && from.x <= to.x,
        });

  (int min, int max) bounds() {
    return switch (direction) {
      Dir.north || Dir.south => (from.y, to.y),
      Dir.west || Dir.east => (from.x, to.x),
    };
  }

  int length() {
    return switch (direction) {
      Dir.north || Dir.south => (to.y - from.y),
      Dir.east || Dir.west => (to.x - from.x),
    }
        .abs();
  }

  @override
  String toString() {
    return "Segement(from: $from, to: $to, dir: $direction, length: ${length()})";
  }

  @override
  void render() {
    var (min, max) = bounds();
    if (direction case Dir.south || Dir.west) {
      (min, max) = (max, min);
    }
    for (var i = min; i <= max; i++) {
      switch (direction) {
        case Dir.north || Dir.south:
          moveCursor(i, from.x);
        case Dir.east || Dir.west:
          moveCursor(from.y, i);
      }
      stdout.write(direction.toString());
    }
  }
}

class Pos implements Renderable {
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
    return switch (dir) {
      Dir.north => Pos(x - 1, y), // uh we are in Austrailia ig
      Dir.south => Pos(x + 1, y),
      Dir.east => Pos(x, y + 1),
      Dir.west => Pos(x, y - 1),
    };
  }

  isOn(Board board) {
    return x >= 1 && y >= 1 && x <= board.height && y <= board.width;
  }

  @override
  void render() {
    moveCursor(y, x); // TODO does this work?
    stdout.write(Renderable.RED_X);
  }
}

enum Dir {
  north,
  south,
  west,
  east;

  @override
  String toString() {
    return switch (this) {
      Dir.north || Dir.south => "|",
      Dir.east || Dir.west => "-",
    };
  }
}

class Guard {
  Pos pos;
  Dir dir;
  Set<Pos> visited;

  Guard(this.pos, this.dir, this.visited);
  Guard.fromInts(int x, int y) : this(Pos(x, y), Dir.north, {Pos(x, y)});
  @override
  String toString() {
    return switch (dir) {
      Dir.north => '^',
      Dir.south => 'v',
      Dir.east => '>',
      Dir.west => '<',
    };
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
      case Dir.east:
        dir = Dir.south;
      case Dir.south:
        dir = Dir.west;
      case Dir.west:
        dir = Dir.north;
    }
  }

  bool isOn(Board board) {
    return pos.isOn(board);
  }

  Pos peek() {
    return pos.move(dir);
  }
}

class Board implements Renderable {
  int width;
  int height;
  Guard guard;
  Map<Pos, bool> obstaclesMap;
  LabelOverseer overseer;

  factory Board._internal(int width, int height, List<Pos> positions,
      Guard guard, LabelOverseer overseer) {
    return Board(width, height, {for (var elem in positions) elem: true}, guard,
        overseer);
  }

  Board(this.width, this.height, this.obstaclesMap, this.guard, this.overseer);

  factory Board.fromString(List<String> lines, LabelOverseer overseer) {
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
    return Board._internal(width, height, positions, guard, overseer);
  }

  /// renders the map on the console
  @override
  void render() {
    visit((pos) {
      stdout.write(getField(pos));
    }, (x) {
      if (x != height) {
        stdout.write("\n");
      }
    });
  }

  void reRender(Pos prev, Pos curr) {
    for (var pos in [prev, curr]) {
      moveCursor(pos.x, pos.y); // this works?
      stdout.write(getField(pos));
    }
    sleep(Duration(milliseconds: 80));
  }

  String getField(Pos pos) {
    if (obstaclesMap.containsKey(pos)) {
      return "#";
    } else if (guard.pos == pos) {
      return guard.toString();
    } else if (guard.visited.contains(pos)) {
      return Renderable.RED_X;
    } else {
      return ".";
    }
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
        buf.write(getField(Pos(x, y)));
      }
      if (x != height) {
        buf.writeln();
      }
    }
    return buf.toString();
  }
}
