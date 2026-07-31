import { describe, it, expect } from "vitest";
import { renderToStaticMarkup } from "react-dom/server";
import { RefusalPresentation } from "../../components/refusal-presentation";
import { REFUSAL_CODES } from "../../lib/domain/refusal";

describe("RefusalPresentation (real React SSR render, real RDF-sourced codes)", () => {
  it("renders all 16 real refusal codes without throwing, each producing distinct real markup", () => {
    expect(REFUSAL_CODES.length).toBe(16);
    const outputs = REFUSAL_CODES.map((code) =>
      renderToStaticMarkup(<RefusalPresentation code={code} />)
    );
    // every real code must produce markup containing that exact code as a data attribute
    REFUSAL_CODES.forEach((code, i) => {
      expect(outputs[i]).toContain(`data-code="${code}"`);
    });
    // all 16 outputs must be distinct (proves genuine per-code rendering, not a constant)
    expect(new Set(outputs).size).toBe(16);
  });

  it("renders an optional reason when provided", () => {
    const html = renderToStaticMarkup(
      <RefusalPresentation code="SANDBOX_TIMEOUT" reason="exceeded 10s bound" />
    );
    expect(html).toContain("exceeded 10s bound");
  });
});
