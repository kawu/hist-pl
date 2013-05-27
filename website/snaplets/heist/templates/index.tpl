<meta charset="utf-8">
<html>


  <head>
    <title>Słownik dawnej polszczyzny</title>
    <link rel="stylesheet" href="/public/css/style.css">
  </head>

  
  <body onload="init();">
    <div style="width:90%" class="centered">

      <h2>Słownik dawnej polszczyzny</h2>
      <hr><br>

      <div id="left_col">
        <form method="post" action="">
        <textarea placeholder="Wpisz tekst" id="input"
            name="input" rows="1" maxlength="10000"
            autofocus="autofocus"><ana-input/></textarea><br>
        <br><input type="submit" value="Znakuj"/>
        </form>
      </div>

      <div id="right_col" padding="5">
        <ana-output/>
      </div>

    </div>
  </body>

<script type="text/javascript">
  var observe;
  if (window.attachEvent) {
      observe = function (element, event, handler) {
          element.attachEvent('on'+event, handler);
      };
  }
  else {
      observe = function (element, event, handler) {
          element.addEventListener(event, handler, false);
      };
  }
  function init () {
      var text = document.getElementById('input');
      function resize () {
          text.style.height = 'auto';
          text.style.height = Math.max((20 + text.scrollHeight), 400) + 'px';
      }
      /* 0-timeout to get the already changed text */
      function delayedResize () {
          window.setTimeout(resize, 0);
      }
      observe(text, 'change',  resize);
      observe(text, 'cut',     delayedResize);
      observe(text, 'paste',   delayedResize);
      observe(text, 'drop',    delayedResize);
      observe(text, 'keydown', delayedResize);
  
      text.focus();
      /* text.select(); */
      resize();
  }
</script>

</html>
