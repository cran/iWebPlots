/**
Implemented by Chatzimichali Eleni-Anthippi

Developers can modify the contents of this script in order to extend the functionality of the Image Maps

*/


$(document).ready(function()
{  		
	
	$("head").append("<link type='text/css' rel='stylesheet' href='css/demo_page.css'><br>"+
			 "<link type='text/css' rel='stylesheet' href='css/demo_table_jui.css'><br>"+
			 "<link type='text/css' rel='stylesheet' href='css/demo_table.css'>");
	
	loadData();
} );


// load data in DataTable
function loadData()
{
	$.ajax({
		"dataType": 'text',
		"type": "GET",
		"url": "json.txt", 
		"success": function (dataStr)
		{
			var data = eval( '('+dataStr+')' );
			
			$("#dataTable").dataTable({
				"aaSorting": [[0, "asc"]], 
				"aaData": data.aaData,
				"aoColumns":  data.aoColumns
			});
		}
	} );
}

// filter dataTable columns
function fnFilterColumn(ind)
{
	$('#dataTable').dataTable().fnFilter(ind, 0, false, true);
}