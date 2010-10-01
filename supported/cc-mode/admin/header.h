<!-- -*- html -*- -->
<html>
<head>
<title>CC Mode<?php if (isset ($title)) echo " - " . $title; ?></title>
</head>

<body bgcolor="#ffffff" text="#000000" link="#6060ff" vlink="#4848d0">

<table cellpadding="4" cellspacing="2" width="100%">
  <tr>
    <td><font size="+2"><b>CC Mode</b></font></td>
    <td><font size="+1"><b><?php 
      if (isset ($title)) echo $title; else echo "&nbsp;";
    ?></b></font></td>
  </tr>
  <tr><td colspan="2"><hr noshade /></td></tr>
  <tr valign="top">
    <td bgcolor="#e8f0f8">
      <?php
	if (!isset ($menufiles)) $menufiles = array ("links.h");
	for ($i = 0; $i < sizeof ($menufiles); $i++) {
	  include ($menufiles[$i]);
	}
	for ($i = 0; $i < 35; $i++) echo "&nbsp;";
      ?>
    </td>
    <td width="100%">
