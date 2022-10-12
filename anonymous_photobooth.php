<?php
$pagetitle= "Mollie and Alex's Wedding!";
require_once('../classes/portfolioimage.php');
$all= array();
$eventImages= array('all'=>$all);

//Count images in directory:
$directory = "eventimg/alexandmollie2022/all/";
$filecount = 0;
$files = glob($directory . "*.jpg");
if ($files){
 $filecount = count($files);
}

for($i=1; $i<=$filecount; $i++) {
	$eventImages['all'][$i]= new PhotoboothImage($i, 'all', 'alexandmollie2022');
}
$eventImageDirectory= "biophotobooth";

$event_thumbnails= '';

foreach($eventImages as $category) {
	foreach($category as $image) {
		$thumb= $image->thumb;
		$img_id= $image->id;
		$cat= $image->category;
		$full= strtolower($image->source);
		$portfolio_thumbnails = "<a href='#'><img id='$img_id' src='$full' class='portfolio_thumb $cat' onClick=\"boothDisplay('$img_id'); return false;\" /></a>\n\t\t" . $portfolio_thumbnails;
	}//end second level iteration
}//end top level iteration

$previewimage = $full;
$bodyCopy= <<<_BodyCopy
<section id='bodyCopy'>
	<script>
		document.onkeydown = function (e) {
		switch (e.keyCode) {
			case 39:
				boothDisplayNext($filecount);
				break;
			case 37:
				boothDisplayPrev();
				break;
			default:
				return;
			}
		}
	</script>
	<h2>Celebrating Alex and Mollie! 10/1/2022</h2>
	<!--<p>To download all photos, click <a href='/eventimg/alexandmollie2022/alexandmollie2022.zip'>here</a> (___MB).</p>-->
	<img id='portfolio_display' src='eventimg/alexandmollie2022/all/all-full-$filecount.jpg' class='biggerdisplay'/>
	<p class = 'gallery_directions'>Use the left and right arrow keys to flip between images, or click a thumbnail to enlarge. </p>
	<div id='portfolio_thumbs'>
		$portfolio_thumbnails
	</div><!--portfolio_thumbs-->
	<p class='clear'></p>
</section><!--bodyCopy-->
_BodyCopy;
require_once('../template.php');
?>