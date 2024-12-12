/*
    Sovling Day 6 of AoC 2024
*/

import 'dart:io';

Future<void> main(List<String> arguments) async {
  // var map = File("../input/day06.txt");
  // var map = File("./test.txt");
  var map = File("./test.2.txt");
  var lines = await map.readAsLines();
  var board = Board.fromString(lines, LabelOverseer.get());

  // Renderable.withOtherBuffer(() {
  //   var res = board.partOne(withGraphics: true);
  //   moveCursor(12, 1);
  //   stdout.write(res);
  //   var line = 15;
  //   for (final s in board.segments) {
  //     s.render();
  //     moveCursor(line++, 1);
  //     stdout.write(s.label);
  //   }
  //   sleep(Duration(minutes: 3));
  // });
  print(board.partTwo());
}

class LabelOverseer {
  Label _last = Label(0);
  // ignore: non_constant_identifier_names
  static final LabelOverseer INSTANCE = LabelOverseer();
  Label next() {
    var label = Label(_last.id + 1);
    var tmp = _last;
    _last = label;
    return tmp;
  }

  static LabelOverseer get() {
    return INSTANCE;
  }
}

class Label {
  final int id;
  Label(this.id);
  @override
  String toString() {
    return "Label(id: $id)";
  }

  bool isEarlier(Label label) {
    return id < label.id;
  }
}

void moveCursor(int line, int column) {
  // idk why line must be modified
  // stdout.write("\x1b[${line + 1};${column}H");
  stdout.write("\x1b[$line;${column}H");
}

abstract interface class Renderable {
  void render();
  // ignore: non_constant_identifier_names
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

class SegmentBuilder {
  Pos? from;
  Pos? to;
  Dir direction;

  SegmentBuilder(this.direction);

  SegmentBuilder from_(Pos from) {
    this.from = from;
    return this;
  }

  SegmentBuilder to_(Pos to) {
    this.to = to;
    return this;
  }

  Segment build(LabelOverseer overseer, [Segment? next]) {
    if (from != null && to != null) {
      var seg = Segment(
          from: from as Pos,
          to: to as Pos,
          direction: direction,
          label: overseer.next());
      seg.next = next;
      return seg;
    }
    throw "from $from or to $to is null";
  }

  @override
  String toString() {
    return "SegmentBuilder(from: $from, to: $to, dir: $direction)";
  }
}

typedef Segments = List<Segment>;
typedef SegmentPartiton = List<Segments>; // this list has 4 elements

class Segment implements Renderable {
  final Pos from;
  final Pos to;
  final Dir direction;
  final Label label;
  Segment? next;

  Segment(
      {required this.from,
      required this.to,
      required this.direction,
      required this.label})
      : assert(switch (direction) {
          Dir.north => from.x == to.x && from.y >= to.y,
          Dir.south => from.x == to.x && from.y <= to.y,
          Dir.west => from.y == to.y && from.x <= to.x,
          Dir.east => from.y == to.y && from.x >= to.x,
        });

  int end() {
    return switch (direction) {
      Dir.north || Dir.south => to.y,
      Dir.west || Dir.east => to.x,
    };
  }

  int start() {
    return switch (direction) {
      Dir.north || Dir.south => from.y,
      Dir.west || Dir.east => from.x,
    };
  }

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
            .abs() +
        1;
  }

  static Segment link(List<Segment> segments) {
    var curr = segments[0];
    for (var i = 1; i < segments.length; i++) {
      curr.next = segments[i];
      curr = segments[i];
    }
    return segments[0];
  }

  @override
  String toString() {
    return "Segement(from: $from, to: $to, dir: ${direction.str()}, length: ${length()})";
  }

  @override
  void render() {
    var (min, max) = bounds();
    if (direction case Dir.north || Dir.west) {
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

  /// this matches x means this searches for intersections with segments going
  /// 90° turn right
  (Pos, bool) matches(Segment other, Board board) {
    var obstacles = board.obstaclesMap;
    // has to test for:
    //  1. in bounds (ie. not too far up, right, left or down)
    //    1.1. Not be blocked by an obstacle
    //  2. is a previous one
    bool cond3 = other.label.isEarlier(label);
    // var cond3 = true;

    int limit = end();
    int thisStart = start();
    int theirEnd = other.end();
    // in bounds
    // console is flipped on y axis
    bool cond1 = switch (direction) {
      // we're up => other's right
      Dir.north => limit <= other.to.y && theirEnd >= to.x && other.to.y <= thisStart,
      // we're down => other's left
      Dir.south => limit >= other.to.y && theirEnd <= to.x && other.to.y >= thisStart,
      // we're right => other's down
      Dir.east => limit >= other.to.x && theirEnd >= to.y && other.to.x >= thisStart,
      // we're left => other's up
      Dir.west => limit <= other.to.x && theirEnd <= to.y && other.to.x <= thisStart,
    };

    if (!cond3 || !cond1) {
      return (Pos(-1, -1), false);
    }

    Pos intersection = switch (direction) {
      // we're up => other's right
      Dir.north || Dir.south => Pos(to.x, other.to.y),
      // we're right => other's down
      Dir.east || Dir.west => Pos(other.to.x, to.y),
    };

    // not blocked by some obstacle
    List<Pos> line = intersection.line(other.to, other.direction);
    for (final pos in line) {
      if (obstacles.containsKey(pos)) {
        return (Pos(-1, -1), false);
      }
    }

    return (intersection, true);
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
      Dir.north => Pos(x, y - 1), // console coords are flipped on the x axis
      Dir.south => Pos(x, y + 1),
      Dir.east => Pos(x + 1, y),
      Dir.west => Pos(x - 1, y),
    };
  }

  isOn(Board board) {
    return x >= 1 && y >= 1 && x <= board.height && y <= board.width;
  }

  List<Pos> line(Pos other, Dir dir) {
    var curr = this;
    List<Pos> ret = [];
    while (curr != other) {
      curr = curr.move(dir);
      ret.add(curr);
    }
    return ret;
  }

  @override
  void render() {
    moveCursor(y, x); // line and col
    stdout.write(Renderable.RED_X);
  }

  @override
  String toString() {
    return "Pos($x, $y)";
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
  Map<Pos, bool> obstaclesMap; // lol this could be a set
  Set<Pos> visited;
  LabelOverseer overseer;
  List<Segment> segments = [];
  Segment? pathRoot;
  bool ranPartOne = false;

  factory Board._internal(int width, int height, List<Pos> positions,
      Guard guard, LabelOverseer overseer) {
    return Board(width, height, {for (var elem in positions) elem: true}, guard,
        overseer, {guard.pos});
  }

  Board(this.width, this.height, this.obstaclesMap, this.guard, this.overseer,
      this.visited);

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
          positions.add(Pos(width, height));
        } else if (c == '^') {
          guard = Guard.fromInts(width, height);
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
    }, (y) {
      if (y != height) {
        stdout.write("\n");
      }
    });
  }

  void reRender(Pos prev, Pos curr) {
    for (var pos in [prev, curr]) {
      moveCursor(pos.y, pos.x);
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

  int partOne({bool withGraphics = false}) {
    if (withGraphics) {
      render();
    }
    Pos prev = guard.pos;
    SegmentBuilder sb = SegmentBuilder(Dir.north);
    sb.from_(guard.pos);
    while (guard.isOn(this)) {
      prev = guard.pos;
      Pos next = guard.peek();
      if (obstaclesMap.containsKey(next)) {
        guard.rotateRight();
        sb.to_(guard.pos);
        segments.add(sb.build(overseer));
        sb = SegmentBuilder(guard.dir);
        sb.from_(guard.pos);
      }
      guard.move();
      visited.add(guard.pos);
      if (withGraphics) {
        reRender(prev, guard.pos);
      }
    }
    sb.to_(guard.pos.move(guard.dir.opposite()));
    segments.add(sb.build(overseer));
    pathRoot = Segment.link(segments);
    ranPartOne = true;
    return guard.visited.where((pos) => pos.isOn(this)).length;
  }

  int partTwo({bool withGraphics = false}) {
    if (!ranPartOne) {
      partOne(withGraphics: withGraphics);
    }
    Set<Pos> newObstacles = {};
    var partitions = partitionSegments();
    for (var i = 0; i < partitions.length; i++) {
      var partition = partitions[i];
      var next = partitions[switch (i) { 3 => 0, _ => i + 1 }];
      for (final elem in partition) {
        // i.e. for each *up*...
        for (final match in next) {
          // ...we check if it has one intersection with *right*
          var (loc, bl) = elem.matches(match, this);
          if (bl) {
            newObstacles.add(loc);
          }
        }
      }
    }

    return newObstacles.length;
  }

  /// up right down left
  SegmentPartiton partitionSegments() {
    List<Segment> up = [];
    List<Segment> down = [];
    List<Segment> left = [];
    List<Segment> right = [];

    for (final seg in segments) {
      switch (seg.direction) {
        case Dir.north:
          up.add(seg);
        case Dir.south:
          down.add(seg);
        case Dir.east:
          right.add(seg);
        case Dir.west:
          left.add(seg);
      }
    }

    return [up, right, down, left];
  }

  /// visit each node and apply fn on the current position
  void visit(void Function(Pos) fn, void Function(int) outerLoop) {
    for (var y = 1; y <= height; y++) {
      for (var x = 1; x <= width; x++) {
        fn(Pos(x, y));
      }
      outerLoop(y);
    }
  }

  @override
  String toString() {
    final buf = StringBuffer();
    for (var y = 1; y <= height; y++) {
      for (var x = 1; x <= width; x++) {
        buf.write(getField(Pos(x, y)));
      }
      if (y != height) {
        buf.writeln();
      }
    }
    return buf.toString();
  }
}

extension on Dir {
  Dir opposite() {
    return switch (this) {
      Dir.north => Dir.south,
      Dir.south => Dir.north,
      Dir.east => Dir.west,
      Dir.west => Dir.east,
    };
  }

  String str() {
    return switch (this) {
      Dir.north => "north",
      Dir.south => "south",
      Dir.east => "east",
      Dir.west => "west",
    };
  }
}
