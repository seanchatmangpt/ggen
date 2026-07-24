import { describe, it, expect } from "vitest";
import { languageIdForCapability, buildMonacoConfig } from "../../lib/adapters/monaco-adapter";

describe("monaco-adapter (DOM-free logic only)", () => {
  it("maps rust capabilities to the rust language id", () => {
    expect(languageIdForCapability("compile_rust")).toBe("rust");
    expect(languageIdForCapability("execute_rust")).toBe("rust");
    expect(languageIdForCapability("run_cargo_test")).toBe("rust");
  });

  it("maps python capabilities to the python language id", () => {
    expect(languageIdForCapability("compile_python")).toBe("python");
    expect(languageIdForCapability("execute_python")).toBe("python");
    expect(languageIdForCapability("run_pytest")).toBe("python");
  });

  it("builds a real config object after the policy check passes", () => {
    const cfg = buildMonacoConfig({ capability: "execute_python", initialValue: "print(1)" });
    expect(cfg.language).toBe("python");
    expect(cfg.value).toBe("print(1)");
    expect(cfg.readOnly).toBe(false);
  });

  it("NOT TESTED HERE: mounting monaco.editor.create against a real DOM node " +
     "and its web-worker language service -- requires a real browser " +
     "(Playwright, TICKET-039), documented rather than mocked", () => {
    expect(true).toBe(true);
  });
});
