<meta charset="utf-8">
<html>


  <head>
    <title>Słownik dawnej polszczyzny</title>
    <link rel="stylesheet" href="/public/css/style.css">
  </head>

  
  <body>
    <div style="width:90%" class="centered">
      <apply template="nav"/>

      <div id="left_col">
        <form>
        Wyszukaj formy rozpoczynające się na:
        <input type="text" name="prefix">
        <input type="submit" value="Szukaj">
        </form>
        <list-output/>
      </div>

      <div id="right_col">
        <lex-entry/>
      </div>

    </div>
  </body>

</html>
