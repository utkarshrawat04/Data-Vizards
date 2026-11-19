(() => {
  'use strict';
  const $ = (id) => document.getElementById(id);
  const help = $('helpModal');
  let previousFocus = null;
  function setHelp(open) {
    previousFocus = open ? document.activeElement : previousFocus;
    help.classList.toggle('open', open);
    help.setAttribute('aria-hidden', String(!open));
    document.body.style.overflow = open ? 'hidden' : '';
    if (open) $('helpClose').focus();
    else if (previousFocus && previousFocus.focus) { previousFocus.focus(); previousFocus = null; }
  }
  $('helpOpen').addEventListener('click', () => setHelp(true));
  $('helpClose').addEventListener('click', () => setHelp(false));
  $('helpDone').addEventListener('click', () => setHelp(false));
  document.addEventListener('keydown', (e) => { if (e.key === 'Escape' && help.classList.contains('open')) setHelp(false); });
  $('reloadExplore').addEventListener('click', () => { const frame = $('exploreFrame'); frame.src = frame.src; });
  const navLinks = Array.from(document.querySelectorAll('.nav a'));
  const sections = Array.from(document.querySelectorAll('main section[id]'));
  const progress = $('progress');
  let ticking = false;
  function updateScroll() {
    const max = document.documentElement.scrollHeight - window.innerHeight;
    progress.style.width = (max > 0 ? (window.scrollY / max) * 100 : 0) + '%';
    let current = 'home';
    for(const section of sections) if(section.getBoundingClientRect().top <= window.innerHeight * .35) current = section.id;
    if(current === 'intro') current = 'home';
    navLinks.forEach(link => { const isActive = link.getAttribute('href') === '#' + current; link.classList.toggle('active', isActive); if(isActive) link.setAttribute('aria-current','location'); else link.removeAttribute('aria-current'); });
    ticking = false;
  }
  window.addEventListener('scroll', () => { if(!ticking) { requestAnimationFrame(updateScroll); ticking = true; } }, {passive:true});
  updateScroll();
  if('IntersectionObserver' in window && !window.matchMedia('(prefers-reduced-motion: reduce)').matches) {
    const observer = new IntersectionObserver(entries => entries.forEach(entry => { if(entry.isIntersecting) {entry.target.classList.add('visible'); observer.unobserve(entry.target);} }), {threshold:.06});
    document.querySelectorAll('.chapter-heading,.intro-grid,.stat-grid,.chapter-tiles,.viz-slot,.story-aside,.method-note').forEach(el => {el.classList.add('reveal'); observer.observe(el);});
  }
  const config = window.KOLKATA_DASHBOARDS || {};
  for(const [key,url] of Object.entries(config)) {
    const host = $('embed-' + key);
    if(!host || typeof url !== 'string' || !url.trim()) continue;
    const trimmed = url.trim();
    if(!/^(https:\/\/|\.\.?\/|dashboards\/)/i.test(trimmed)) {console.warn('Unsupported dashboard URL:',key); continue;}
    const iframe = document.createElement('iframe');
    iframe.className = 'embedded-shiny';
    iframe.title = 'Kolkata ' + key + ' interactive visualisation';
    iframe.loading = 'lazy';
    iframe.referrerPolicy = 'strict-origin-when-cross-origin';
    iframe.src = trimmed;
    host.replaceChildren(iframe);
  }
})();
