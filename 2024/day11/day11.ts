
function parse(input: string) /* Returns a hashmap, but I can't figure out the type */ {
    const parts = input.split(" ");
    let ret = {};
    parts.forEach((num) => {
        ret[num] = 1;
    });
    return ret;
}

function rule(num: number) {
    if (num == 0) {
        return { 1: 1 }
    }
    if (num.toString().length % 2 === 0) {
        const str = num.toString();
        const fh = str.slice(0, str.length / 2);
        const sh = str.slice(str.length / 2, str.length);
        if (parseInt(fh) == parseInt(sh)) {
            return {
                [parseInt(fh)]: 2,
            }
        } else {
            return {
                [parseInt(fh)]: 1,
                [parseInt(sh)]: 1,
            }
        }
    }
    const newNum = num * 2024;
    return { [newNum]: 1 }
}

interface Merge { (a: number, b: number): number; };

// I have to write this myself?
// any so it doesnt cry about the types
function merge(target: Record<string, number>, source: Record<string, number>, fn: any) {
    for (const [key, value] of Object.entries(source)) {
        if (parseInt(key) in target) {
            target[parseInt(key)] = fn(target[parseInt(key)], value)
        } else {
            target[parseInt(key)] = value
        }
    }
}

function sprintNotNull(data: Record<string, number>): string {
    let ret = "";
    for (const [key, value] of Object.entries(data)) {
        if (value != 0) {
            ret += `${key}:${value} `;
        }
    }
    return ret;
}

function summ(foo: any) {
    let sum = 0;
    for (const key in foo) {
        sum += foo[parseInt(key)];
    }
    return sum;
}

function loop(parsed: Record<string, number>, times: number) {
    let foo = structuredClone(parsed);
    for (let i = 0; i < times; i++) {
        let tmp = structuredClone(foo);
        for (const key in tmp) {
            if (tmp[parseInt(key)] == 0) { continue; }

            for (let j = 0; j < tmp[parseInt(key)]; j++) {
                const ruled = rule(parseInt(key));
                merge(foo, ruled, (old, neu) => old + neu);
                if (foo[parseInt(key)] > 1) {
                    foo[parseInt(key)] -= 1;
                } else {
                    delete foo[parseInt(key)];
                }
            }

        }
    }
    return summ(foo)
}

function main() {
    // sorry but I couldn't get the file reading to work... 
    const input = "30 71441 3784 580926 2 8122942 0 291";
    // const input = "125 17";
    const parsed = parse(input);
    console.log(loop(parsed, 25));
}

main()
