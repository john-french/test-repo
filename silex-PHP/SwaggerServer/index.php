<?php
require_once __DIR__ . '/vendor/autoload.php';

use Symfony\Component\HttpFoundation\Request;
use Symfony\Component\HttpFoundation\Response;
use Silex\Application;

$app = new Silex\Application();


$app->POST('/inventory', function(Application $app, Request $request) {
            
            
            return new Response('How about implementing addInventory as a POST method ?');
            });


$app->GET('/inventory', function(Application $app, Request $request) {
            $search_string = $request->get('search_string');    $skip = $request->get('skip');    $limit = $request->get('limit');    
            
            return new Response('How about implementing searchInventory as a GET method ?');
            });


$app->run();
