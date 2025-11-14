// Populate the sidebar
//
// This is a script, and not included directly in the page, to control the total size of the book.
// The TOC contains an entry for each page, so if each page includes a copy of the TOC,
// the total size of the page becomes O(n**2).
class MDBookSidebarScrollbox extends HTMLElement {
    constructor() {
        super();
    }
    connectedCallback() {
        this.innerHTML = '<ol class="chapter"><li class="chapter-item expanded affix "><a href="index.html">ggen Documentation</a></li><li class="chapter-item expanded affix "><li class="spacer"></li><li class="chapter-item expanded "><a href="guides/install.html"><strong aria-hidden="true">1.</strong> Installation</a></li><li class="chapter-item expanded "><a href="guides/quickstart.html"><strong aria-hidden="true">2.</strong> Quick Start (5 Minutes)</a></li><li class="chapter-item expanded "><a href="whats-new-2.5.0.html"><strong aria-hidden="true">3.</strong> What&#39;s New in v2.5.0</a></li><li class="chapter-item expanded "><a href="concepts/rdf-shacl-sparql.html"><strong aria-hidden="true">4.</strong> RDF, SHACL, and SPARQL</a></li><li class="chapter-item expanded "><a href="concepts/projections.html"><strong aria-hidden="true">5.</strong> Semantic Projections</a></li><li class="chapter-item expanded "><a href="concepts/determinism.html"><strong aria-hidden="true">6.</strong> Deterministic Generation</a></li><li class="chapter-item expanded "><a href="concepts/frontmatter.html"><strong aria-hidden="true">7.</strong> Frontmatter</a></li><li class="chapter-item expanded "><a href="tutorials/ontology-driven-workflow.html"><strong aria-hidden="true">8.</strong> Ontology-Driven Workflow</a></li><li class="chapter-item expanded "><a href="guides/templates.html"><strong aria-hidden="true">9.</strong> Templates</a></li><li class="chapter-item expanded "><a href="guides/ai-guide.html"><strong aria-hidden="true">10.</strong> AI-Powered Generation</a></li><li class="chapter-item expanded "><a href="guides/hooks.html"><strong aria-hidden="true">11.</strong> Hooks &amp; Automation</a></li><li class="chapter-item expanded "><a href="guides/production-ready.html"><strong aria-hidden="true">12.</strong> Production Readiness</a></li><li class="chapter-item expanded "><a href="reference/cli.html"><strong aria-hidden="true">13.</strong> CLI Reference</a></li><li class="chapter-item expanded "><a href="reference/troubleshooting.html"><strong aria-hidden="true">14.</strong> Troubleshooting</a></li><li class="chapter-item expanded "><a href="marketplace.html"><strong aria-hidden="true">15.</strong> Marketplace Guide</a></li><li class="chapter-item expanded "><a href="registry/index.html"><strong aria-hidden="true">16.</strong> Registry API</a></li><li class="chapter-item expanded "><a href="diagrams/README_marketplace_diagrams.html"><strong aria-hidden="true">17.</strong> System Diagrams</a></li><li class="chapter-item expanded "><a href="ai-integration/GENAI_GGEN_INTEGRATION_PLAN.html"><strong aria-hidden="true">18.</strong> AI Integration Overview</a></li><li class="chapter-item expanded "><a href="ai-integration/GENAI_INTEGRATION_STATUS.html"><strong aria-hidden="true">19.</strong> Current Integration Status</a></li><li class="chapter-item expanded "><a href="ai-integration/AI_INTEGRATION_CLARIFICATION.html"><strong aria-hidden="true">20.</strong> AI Integration Clarification</a></li><li class="chapter-item expanded "><a href="ai-integration/GENAI_OLLAMA_INTEGRATION.html"><strong aria-hidden="true">21.</strong> Ollama Integration Guide</a></li><li class="chapter-item expanded "><a href="ai-integration/MULTI_PROVIDER_ANALYSIS.html"><strong aria-hidden="true">22.</strong> Multi-Provider Analysis</a></li><li class="chapter-item expanded "><a href="ai-integration/RUNTIME_MODEL_CONFIG.html"><strong aria-hidden="true">23.</strong> Runtime Model Configuration</a></li><li class="chapter-item expanded "><a href="ai-integration/BUILD_OPTIMIZATION.html"><strong aria-hidden="true">24.</strong> Build Optimization</a></li><li class="chapter-item expanded "><a href="ai-integration/CARGO_BEST_PRACTICES.html"><strong aria-hidden="true">25.</strong> Cargo Best Practices</a></li><li class="chapter-item expanded "><a href="advanced/calculus.html"><strong aria-hidden="true">26.</strong> Mathematical Foundations</a></li><li class="chapter-item expanded "><a href="advanced/dx-features.html"><strong aria-hidden="true">27.</strong> Developer Experience</a></li><li class="chapter-item expanded "><a href="advanced/gpack-development.html"><strong aria-hidden="true">28.</strong> Gpack Development</a></li><li class="chapter-item expanded "><a href="examples/cli-subcommand-multi.html"><strong aria-hidden="true">29.</strong> Multi-language CLI</a></li><li class="chapter-item expanded "><a href="examples/cli-subcommand.html"><strong aria-hidden="true">30.</strong> Rust CLI Subcommand</a></li><li class="chapter-item expanded "><a href="examples/sql-schema.html"><strong aria-hidden="true">31.</strong> SQL Schema</a></li></ol>';
        // Set the current, active page, and reveal it if it's hidden
        let current_page = document.location.href.toString().split("#")[0].split("?")[0];
        if (current_page.endsWith("/")) {
            current_page += "index.html";
        }
        var links = Array.prototype.slice.call(this.querySelectorAll("a"));
        var l = links.length;
        for (var i = 0; i < l; ++i) {
            var link = links[i];
            var href = link.getAttribute("href");
            if (href && !href.startsWith("#") && !/^(?:[a-z+]+:)?\/\//.test(href)) {
                link.href = path_to_root + href;
            }
            // The "index" page is supposed to alias the first chapter in the book.
            if (link.href === current_page || (i === 0 && path_to_root === "" && current_page.endsWith("/index.html"))) {
                link.classList.add("active");
                var parent = link.parentElement;
                if (parent && parent.classList.contains("chapter-item")) {
                    parent.classList.add("expanded");
                }
                while (parent) {
                    if (parent.tagName === "LI" && parent.previousElementSibling) {
                        if (parent.previousElementSibling.classList.contains("chapter-item")) {
                            parent.previousElementSibling.classList.add("expanded");
                        }
                    }
                    parent = parent.parentElement;
                }
            }
        }
        // Track and set sidebar scroll position
        this.addEventListener('click', function(e) {
            if (e.target.tagName === 'A') {
                sessionStorage.setItem('sidebar-scroll', this.scrollTop);
            }
        }, { passive: true });
        var sidebarScrollTop = sessionStorage.getItem('sidebar-scroll');
        sessionStorage.removeItem('sidebar-scroll');
        if (sidebarScrollTop) {
            // preserve sidebar scroll position when navigating via links within sidebar
            this.scrollTop = sidebarScrollTop;
        } else {
            // scroll sidebar to current active section when navigating via "next/previous chapter" buttons
            var activeSection = document.querySelector('#sidebar .active');
            if (activeSection) {
                activeSection.scrollIntoView({ block: 'center' });
            }
        }
        // Toggle buttons
        var sidebarAnchorToggles = document.querySelectorAll('#sidebar a.toggle');
        function toggleSection(ev) {
            ev.currentTarget.parentElement.classList.toggle('expanded');
        }
        Array.from(sidebarAnchorToggles).forEach(function (el) {
            el.addEventListener('click', toggleSection);
        });
    }
}
window.customElements.define("mdbook-sidebar-scrollbox", MDBookSidebarScrollbox);
