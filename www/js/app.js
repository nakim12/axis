(function () {
  function setHome(isHome) {
    document.body.classList.toggle('app-home-tab', !!isHome);
  }

  function showTab(value) {
    var link = document.querySelector('.navbar a[data-value="' + value + '"]');
    if (!link) return;
    if (window.bootstrap && bootstrap.Tab) bootstrap.Tab.getOrCreateInstance(link).show();
    else $(link).tab('show');
  }

  $(document).on('shiny:connected', function () {
    Shiny.addCustomMessageHandler('setHomeTab', function (msg) {
      setHome(msg.home);
    });
  });

  $(document).on('shiny:inputchanged', function (e) {
    if (e.name === 'main_nav') setHome(e.value === 'home');
  });

  // The wordmark is the way back to Home; the Home tab itself is hidden from the pill.
  $(document).on('click', '.navbar .navbar-brand', function (e) {
    e.preventDefault();
    showTab('home');
  });

  // Each page starts at the top, unless the user is already near it.
  $(document).on('shown.bs.tab', '.navbar a[data-bs-toggle="tab"], .navbar a[data-toggle="tab"]', function () {
    if (window.scrollY > 0) window.scrollTo({ top: 0, behavior: 'auto' });
    $('.navbar-collapse.show').removeClass('show');
  });
})();
