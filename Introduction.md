---
title: "Introduction"
output:
  html_document:
    df_print: paged
---

### Introduction


 <style>
  .col2 {
    columns: 2 100px;         /* number of columns and width in pixels*/
    -webkit-columns: 2 100px; /* chrome, safari */
    -moz-columns: 2 100px;    /* firefox */
  }
  .col3 {
    columns: 3 100px;
    -webkit-columns: 3 100px;
    -moz-columns: 3 100px;
  }

.column-left{
  float: left;
  width: 38%;
  text-align: left;
}
.column-center{
  display: inline-block;
  width: 33%;
  text-align: center;
}
.column-right{
  float: right;
  width: 33%;
  text-align: right;
}
</style>

<div class="column-left">
<center> <img src= "www/gif/WeepyEmpty.gif" width="100"/> </center>
<font size="2"><center> source:gfycat.com</center></font> 
<!--https://gfycat.com/weepyemptycaracal-www-gif-vif-com -->
</div>

<font size="4">
Coronavirus disease 2019 (COVID-19) is an infectious disease caused by severe acute respiratory syndrome coronavirus 2 (SARS-CoV-2) [(1)](https://www.who.int/emergencies/diseases/novel-coronavirus-2019/technical-guidance/naming-the-coronavirus-disease-%28covid-2019%29-and-the-virus-that-causes-it). On 31 December 2019, the WHO China Country Office was informed the first cases of Coronavirus disease 2019 (COVID-19) in Wuhan City, Hubei Province of China [(2)](https://www.who.int/docs/default-source/coronaviruse/situation-reports/20200121-sitrep-1-2019-ncov.pdf?sfvrsn=20a99c10_4). 

COVID-19 disease is spreading rapidly throughout the world. The outbreak was declared a public health emergency of international concern on 30 January 2020  [(3)](https://www.who.int/emergencies/diseases/novel-coronavirus-2019/events-as-they-happen).WHO declared the outbreak a global pandemic on March 11, 2020 [(4)](https://www.who.int/dg/speeches/detail/who-director-general-s-opening-remarks-at-the-media-briefing-on-covid-19---11-march-2020).
In response to this ongoing public health emergency, the Center for Systems Science and Engineering [(CSSE)](https://systems.jhu.edu/) at Johns Hopkins University, Baltimore, MD, USA,  developed an online interactive [dashboard](https://coronavirus.jhu.edu/map.html), to visualize and track reported cases of COVID-19 in real time [(5)](https://www.thelancet.com/journals/laninf/article/PIIS1473-3099%2820%2930120-1/fulltext).
They developed the dashboard to provide researchers, public health authorities, and the general public with a tool to track the outbreak. Johns Hopkins University is collecting COVID-19 outbreak data from several sources including including [WHO](https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports), U.S. [CDC](https://www.cdc.gov/coronavirus/2019-ncov/index.html), [ECDC](https://www.ecdc.europa.eu/en/home), China CDC  [(CCDC)](http://www.chinacdc.cn/en/), [NHC](http://www.nhc.gov.cn/yjb/s3578/new_list.shtml) and [DXY](https://3g.dxy.cn/newh5/view/pneumonia?scene=2&clicktime=1579582238&enterid=1579582238&from=singlemessage&isappinstalled=0), as well as city-level and state-level health authorities. They made all collected data freely available, through a [GitHub repository](https://github.com/CSSEGISandData/COVID-19) to be used for further analyzing by researchers. However, the data in this repository requires pre-processing or more aggregation processing before further use, which I thought many researchers may not have the patience to do such preparations. In addition, Johns Hopkins dashboard does not allow options like downloading of processed data and analyzing of date at provincial-scale.
<!-- 
or allowing the use of epidemiological models such as SEIR to be applied to country data for approximation of epidemiological parameters.
--> 
Therefore, developing a dashboard with more capabilities to assist the researchers in analyzing the COVID-19 pandemic situation for countries or provinces is useful.</font>  

### What this dashboard can do?
<font size="4">
In this new dashboard the pandemic data was directly called from GitHub repository of Johns Hopkins University and following capabilities are provided:

* Producing the ranking table of cumulative and new cases of COVID-19 for countries. 
* Producing charts of cumulative and daily trends of epidemic for world, countries and provinces.
* Producing charts of death rate for world, countries and provinces. 
  * Death rate was determined based on [The Lancet article](https://www.thelancet.com/journals/laninf/article/PIIS1473-3099%2820%2930195-X/fulltext) by dividing the number of deaths on a given day by the number of patients with confirmed COVID-19 infection 14 days before.
* Possibility to download the aggregated datasets by world, countries and provinces.
* Creating a dynamic map to show the trend of COVID-19 by confirmed, recovered and deaths cases.
* Producing bar-plot and ranking the countries in terms of cumulative and daily data by confirmed, recovered and deaths cases with population adjustment. 
* Formulation of a simple version of an SEIR model to reflect the disease dynamics 
* Estimation of epidemiological parameters including transmission rate ($\beta$), incubation rate ($\sigma$), recovery rate ($\gamma$) and reproduction number ($R_0$) 
* Assessing the potential effect of social distancing intervention on COVID-19 spread using SEIR model

</font>

### Request for new ideas
<font size="4">
If you have a cool idea and want to add it to this tool, email me at m.hadi1981@gmail.com. Other my contact links are available below this page.
</font>


<center> <img src= "www/Corona.png" width="200"/> </center>
<font size="2"><center> Drawing of my 5-year-old daughter</center></font> 
 
 
 
 
