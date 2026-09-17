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
  const currentIndex = pages.findIndex((page) => page.file === path);
  const currentPage = currentIndex >= 0 ? pages[currentIndex] : null;
  const prevPage = currentIndex > 0 ? pages[currentIndex - 1] : null;
  const nextPage = currentIndex >= 0 && currentIndex < pages.length - 1 ? pages[currentIndex + 1] : null;

  document.body.classList.add("gitbook-ready");

  const legacyNav = main.querySelector(":scope > .line-block");
  if (legacyNav) legacyNav.classList.add("gitbook-legacy-nav");

  const titleNode = document.querySelector("#title-block-header .title");
  if (titleNode && currentPage) titleNode.textContent = currentPage.title;
  if (currentPage) document.title = `${currentPage.title} | Moose Research Initiative`;

  const makeLink = ({ href, label, description, className, external }) => {
    const link = document.createElement("a");
    link.href = href;
    if (className) link.className = className;
    if (external) {
      link.target = "_blank";
      link.rel = "noreferrer";
    }
    link.append(label);
    if (description) {
      const small = document.createElement("small");
      small.textContent = description;
      link.appendChild(small);
    }
    return link;
  };

  const makeSection = (label, navClass) => {
    const wrapper = document.createElement("div");
    const heading = document.createElement("div");
    heading.className = "gitbook-section-label";
    heading.textContent = label;
    const nav = document.createElement("nav");
    nav.className = navClass;
    wrapper.appendChild(heading);
    wrapper.appendChild(nav);
    return { wrapper, nav };
  };

  const sidebar = document.createElement("aside");
  sidebar.className = "gitbook-sidebar";

  const brand = document.createElement("div");
  brand.className = "gitbook-brand";
  const eyebrow = document.createElement("span");
  eyebrow.className = "gitbook-eyebrow";
  eyebrow.textContent = "Research Pages";
  const brandTitle = makeLink({ href: "index.html", label: "Moose Research Initiative", className: "gitbook-brand-title" });
  const brandCopy = document.createElement("p");
  brandCopy.className = "gitbook-brand-copy";
  brandCopy.textContent = "GitBook-style navigation for Theme 4 project pages.";
  brand.append(eyebrow, brandTitle, brandCopy);
  sidebar.appendChild(brand);

  const pagesSection = makeSection("Pages", "gitbook-nav");
  pagesSection.nav.setAttribute("aria-label", "Primary page navigation");
  for (const page of pages) {
    const link = makeLink({
      href: page.href,
      label: page.title,
      description: page.description,
      className: currentPage && page.file === currentPage.file ? "active" : ""
    });
    pagesSection.nav.appendChild(link);
  }
  sidebar.appendChild(pagesSection.wrapper);

  const outlineHeadings = Array.from(main.querySelectorAll("h2.anchored, h3.anchored")).filter((heading) => heading.id);
  if (outlineHeadings.length > 0) {
    const outlineSection = makeSection("On this page", "gitbook-outline");
    outlineSection.nav.setAttribute("aria-label", "Page outline");
    for (const heading of outlineHeadings) {
      outlineSection.nav.appendChild(makeLink({ href: `#${heading.id}`, label: heading.textContent || "Section" }));
    }
    sidebar.appendChild(outlineSection.wrapper);
  }

  const linksSection = makeSection("Links", "gitbook-meta-links");
  linksSection.nav.setAttribute("aria-label", "External links");
  for (const link of externalLinks) {
    linksSection.nav.appendChild(makeLink(link));
  }
  sidebar.appendChild(linksSection.wrapper);

  shell.insertBefore(sidebar, shell.firstChild);

  if (!currentPage) return;

  const footer = document.createElement("nav");
  footer.className = "gitbook-page-footer";
  footer.setAttribute("aria-label", "Page navigation");

  const prevSlot = prevPage
    ? makeLink({ href: prevPage.href, label: prevPage.title, className: "gitbook-page-link prev" })
    : document.createElement("span");
  const nextSlot = nextPage
    ? makeLink({ href: nextPage.href, label: nextPage.title, className: "gitbook-page-link next" })
    : document.createElement("span");

  if (prevPage) {
    const label = document.createElement("small");
    label.textContent = "Previous";
    prevSlot.prepend(label);
  }
  if (nextPage) {
    const label = document.createElement("small");
    label.textContent = "Next";
    nextSlot.prepend(label);
  }

  footer.append(prevSlot, nextSlot);
  main.appendChild(footer);
});
