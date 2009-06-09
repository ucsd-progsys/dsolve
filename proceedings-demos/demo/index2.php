<?
$logfile = "log";
$flag = 0;
$demo = "demo_insertsort";

$category = array(
"insertsort"     => "demo_insertsort",
"mergesort"      => "demo_mergesort",
"mergesort_bug"  => "demo_mergesort_bug",
"quicksort"      => "demo_quicksort",
"listtree"       => "demo_listtree",
"combinations"	 => "demo_combinations",
"bst"            => "demo_bst",
"minibdd"        => "demo_minibdd",
"mapreduce"      => "demo_mapreduce",
"splayheap"      => "demo_splayheap"
);

$category = str_replace(" ", " ", $category);

function getWarns($logfile){
  $warns = "";
  $wflag  = 0; //blech
  $fh = fopen($logfile, 'r');
  while (!feof($fh)){
    $s = fgets($fh);
    if (strpos($s,"exec:") !== false){
      $wflag = 0;
    }
    if ($wflag == 1){
      $warns = $warns . $s;
    }
    if (strpos($s,"Errors") !== false){
      $wflag = 1;
    }

  }
  fclose($fh);
  if ($warns == ""){
    $warns = "<h3>Program Safe</h3>";
  } else {
    $warns = "<h3>Warnings</h3> <pre> ".$warns."</pre>";
  }
  return $warns; 
}

function getAnnots($htmlfile){
  $annothtml = "<h3> Annotated Program </h3>" ;
  $annothtml = $annothtml."Hover mouse over variables to see inferred types." ;
  $annothtml = $annothtml.(file_get_contents($htmlfile));
  return $annothtml;
}

function getRawTextFromField($fld){
  return stripslashes($_POST[$fld]);
}

function writeTextFile($fname,$fld){
  $f = fopen($fname, "w");
  fwrite($f,getRawTextFromField($fld));
  fclose($f);
}

  if($_POST['chooseform'] == "1") {
    $demo = $category[$_POST['choosedemo']]; 
  }

  if($_POST['programform'] == "1") {
    $t     = time();
    $tml   = $t   . ".ml";
    $tann  = $t   . ".annot";
    $tq    = $t   . ".quals";
    $thq   = $t   . ".hquals";
    $tmlq  = $t   . ".cpp.mlq";
    $thtml = $tml . ".html"; 
    writeTextFile($thq,'qualifiers');
    writeTextFile($tmlq,'specification');
    writeTextFile($tml,'program');
    $wd = "/var/www/liquid/demo/";
    shell_exec($wd."runcpp.sh  ".$tmlq);
    shell_exec("LD_LIBRARY_PATH=".$wd."lib ".$wd."dsolve2.py -bare -v 0 -no-simple ".$tml." > ".$logfile." 2>&1");
    shell_exec($wd."caml2html2 -ln ".$tml);
    $annothtml = getAnnots($tml.".html");
    $warnhtml  = getWarns($logfile);
    $loghtml   = "<a href=\"".$logfile."\"> <h3>Log</h3> </a>";
    $flag = 1;
    #shell_exec("rm -f ".$t."*");
  }
?>

<html>
<head>
  <title>Dsolve Demo</title>
  <script SRC="boxover.js"></script>
  <style>
    .annotbody {
        font-family:consolas;
        font-size:16px;
	background:#b4eeb4;
        border-left:1px solid #C4D5E3;
        border-right:1px solid #C4D5E3;
        border-bottom:1px solid #C4D5E3;
        padding:10px;
     }
  </style>
</head>
<body>
  <h1>Dsolve Demo</h1>
  <hr />

<h3>Pick a demo</h3>

<form action='<? echo $_SERVER['PHP_SELF']; ?>' 
      method='post'><p>

<select name="choosedemo">
<? foreach ($category as $key => $value){
     if ($value == $demo) { 
       echo '<OPTION selected = "yes" value='.$key.'> '.$key.''; 
     } else{
       echo '<OPTION value='.$key.'> '.$key.''; 
     }
  } 
?>   
<input name='chooseform' type='hidden' value='1'>
<input type='submit' value='choose'>
<input type='button' value='reset' onclick='window.location = "<? echo $_SERVER['PHP_SELF']; ?>"'>
</select>
</form>

<form action='<? echo $_SERVER['PHP_SELF']; ?>' 
      method='post'><p>

<h3>Logical Qualifiers</h3>
<textarea name='qualifiers' rows='3' cols='80'>
<?
  if($flag == 1) {
    echo (getRawTextFromField('qualifiers'));
  }
  else {
    echo (file_get_contents($demo.".hquals"));
  }
?>
</textarea>

<h3>Specification</h3>
<textarea name='specification' rows='5' cols='80'>
<?
  if($flag == 1) {
    echo (getRawTextFromField('specification'));
  }
  else {
    echo (file_get_contents($demo.".cpp.mlq"));
  }
?>
</textarea>

<h3>Ocaml Program</h3>

<textarea name='program' rows='20' cols='80'>
<?
  if($flag == 1) {
    echo (getRawTextFromField('program'));
  }
  else {
    echo (file_get_contents($demo.".ml"));
  }
?>
</textarea>

<br />
<input name='programform' type='hidden' value='1'>
<input type='submit' value='dsolve'>
</p></form>

<hr />
<? echo $warnhtml ?>
<hr />
<? echo $annothtml ?>
<hr />
<? echo $loghtml ?>
</body>
</html>
