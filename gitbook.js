document.addEventListener("DOMContentLoaded", () => {
  const pages = [
    {
      href: "index.html",
      file: "index.html",
      title: "Moose Research Initiative - Theme 4",
      description: "Project overview, team, and research context."
    },
    {
      href: "sim.html",
      file: "sim.html",
      title: "Simulations",
      description: "Survey design findings for fixed-wing and quadcopter workflows."
    },
    {
      href: "dl.html",
      file: "dl.html",
      title: "Deep Learning",
      description: "Thermal imagery workflow for automated wildlife detection."
    }
  ];

  const externalLinks = [
    { href: "https://irsslab.forestry.ubc.ca/", label: "IRSS", external: true },
    { href: "https://friresearch.ca/", label: "fRI Research", external: true },
    { href: "mailto:137965368+hambrecht@users.noreply.github.com", label: "Contact" }
  ];

  const main = document.getElementById("quarto-document-content");
  const shell = document.getElementById("quarto-content");
  if (!main || !shell) return;

  const path = window.location.pathname.split("/").pop() || "index.html";
  const currentIndex = Math.max(0, pages.findIndex((page) => page.file === path));
  const currentPage = pages[currentIndex];
  const prevPage = pages[currentIndex - 1] || null;
  const nextPage = pages[currentIndex + 1] || null;

  document.body.classList.add("gitbook-ready");

  const legacyNav = main.querySelector(":scope > .line-block");
  if (legacyNav) legacyNav.classList.add("gitbook-legacy-nav");

  const titleNode = document.querySelector("#title-block-header .title");
  if (titleNode && currentPage) titleNode.textContent = currentPage.title;
  if (currentPage) document.title = `${currentPage.title} | Moose Research Initiative`;

  const sidebar = document.createElement("aside");
  sidebar.className = "gitbook-sidebar";

  const navMarkup = pages.map((page) => {
    const active = page.file === currentPage.file ? "active" : "";
    return `<a class="${active}" href="${page.href}">${page.title}<small>${page.description}</small></a>`;
  }).join("");

  const outlineLinks = Array.from(main.querySelectorAll("h2.anchored, h3.anchored"))
    .map((heading) => {
      if (!heading.id) return "";
      return `<a href="#${heading.id}">${heading.textContent}</a>`;
    })
    .filter(Boolean)
    .join("");

  const metaLinks = externalLinks.map((link) => {
    const attrs = link.external ? ' target="_blank" rel="noreferrer"' : "";
    return `<a href="${link.href}"${attrs}>${link.label}</a>`;
  }).join("");

  sidebar.innerHTML = `
    <div class="gitbook-brand">
      <span class="gitbook-eyebrow">Research Pages</span>
      <a class="gitbook-brand-title" href="index.html">Moose Research Initiative</a>
      <p class="gitbook-brand-copy">GitBook-style navigation for Theme 4 project pages.</p>
    </div>
    <div>
      <div class="gitbook-section-label">Pages</div>
      <nav class="gitbook-nav" aria-label="Primary page navigation">${navMarkup}</nav>
    </div>
    ${outlineLinks ? `<div><div class="gitbook-section-label">On this page</div><nav class="gitbook-outline" aria-label="Page outline">${outlineLinks}</nav></div>` : ""}
    <div>
      <div class="gitbook-section-label">Links</div>
      <nav class="gitbook-meta-links" aria-label="External links">${metaLinks}</nav>
    </div>
  `;

  shell.insertBefore(sidebar, shell.firstChild);

  const footer = document.createElement("nav");
  footer.className = "gitbook-page-footer";
  footer.setAttribute("aria-label", "Page navigation");
  footer.innerHTML = `
    ${prevPage ? `<a class="gitbook-page-link prev" href="${prevPage.href}"><small>Previous</small>${prevPage.title}</a>` : "<span></span>"}
    ${nextPage ? `<a class="gitbook-page-link next" href="${nextPage.href}"><small>Next</small>${nextPage.title}</a>` : "<span></span>"}
  `;
  main.appendChild(footer);
});
