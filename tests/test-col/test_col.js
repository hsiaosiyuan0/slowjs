import * as std from "std";
import * as os from "os";

function test(name, ...mustInclude) {
	const fds = os.pipe();
	const cwd = import.meta.url
		.slice("file://".length)
		.slice(0, -"test_col.js".length);
	const proj = `${cwd}../..`;
	const qjs = `${proj}/build/qjs/qjs`;

	os.exec([qjs, `${proj}/tests/test-col/${name}.js`], {
		stdout: fds[1],
		block: true,
	});
	os.close(fds[1]);

	const f = std.fdopen(fds[0], "r");
	const bc = f.readAsString();

	for (const stuff of mustInclude) {
		if (!bc.includes(stuff)) {
			throw new Error(
				`failed to test col at ${name}, got |${bc}| excepted |${stuff}|`
			);
		}
	}
}

test("vardec/basic", "test");
