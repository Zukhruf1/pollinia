# Pollinia — Full Shiny App
# Trait-mediated bee-flower interaction explorer

library(shiny)
library(bslib)
library(tidyverse)
library(networkD3)
library(DT)
library(plotly)
library(shinycssloaders)
library(bipartite)
library(pROC)
library(ggraph)
library(igraph)
library(ggrepel)

# ── Startup data loading ───────────────────────────────────────────────────────

setwd("C:/Users/it-ha/pollinia")

rf_model     <- tryCatch(readRDS("C:/Users/it-ha/pollinia/outputs/models/rf_model.rds"),  error=function(e){cat("RF failed:",e$message,"\n");NULL})
lr_model     <- tryCatch(readRDS("C:/Users/it-ha/pollinia/outputs/models/lr_model.rds"),  error=function(e){cat("LR failed:",e$message,"\n");NULL})
svm_model    <- tryCatch(readRDS("C:/Users/it-ha/pollinia/outputs/models/svm_model.rds"), error=function(e){cat("SVM failed:",e$message,"\n");NULL})
benchmark_df <- tryCatch(read.csv("C:/Users/it-ha/pollinia/outputs/models/benchmark_results.csv"), error=function(e){cat("Bench failed:",e$message,"\n");NULL})

cat("=== Pollinia startup ===\n")
cat("RF loaded:        ", !is.null(rf_model),     "\n")
cat("LR loaded:        ", !is.null(lr_model),     "\n")
cat("SVM loaded:       ", !is.null(svm_model),    "\n")
cat("Benchmark loaded: ", !is.null(benchmark_df), "\n")

web <- tryCatch(
  readRDS("C:/Users/it-ha/pollinia/data/processed/network_matrix.rds"),
  error = function(e) {
    data("Safariland", package = "bipartite", envir = environment())
    Safariland
  }
)

sensitivity_df <- tryCatch(
  read_csv("C:/Users/it-ha/pollinia/outputs/models/sensitivity_summary.csv", show_col_types = FALSE),
  error = function(e) NULL
)

flower_risk_df <- tryCatch(
  read_csv("C:/Users/it-ha/pollinia/data/processed/flower_risk.csv", show_col_types = FALSE),
  error = function(e) NULL
)

# Pre-compute network metrics from Safariland
net_metrics_vals <- tryCatch(
  networklevel(web, index = c("connectance", "nestedness", "H2", "modularity")),
  error = function(e) c(connectance=NA, nestedness=NA, H2=NA, modularity=NA)
)
sp_metrics_vals <- tryCatch(
  specieslevel(web, index = c("degree", "betweenness")),
  error = function(e) NULL
)

default_edges <- as.data.frame(web) %>%
  rownames_to_column("flower_species") %>%
  pivot_longer(-flower_species, names_to = "bee_species", values_to = "visit_count") %>%
  filter(visit_count > 0)

# ── UI ─────────────────────────────────────────────────────────────────────────

ui <- page_navbar(
  title = tags$span("Pollinia", tags$span("🐝", class = "bee-nav")),
  theme = bs_theme(
    bg = "#FEFBF0", fg = "#3D3428",
    primary = "#8BAF7C", secondary = "#EF9F27",
    base_font = font_google("Nunito"),
    heading_font = font_google("Nunito"),
    border_radius = "16px"
  ),
  header = tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Nunito:wght@400;500;600;700;800&display=swap",
      rel = "stylesheet"
    ),
    tags$link(rel = "stylesheet", href = "style.css")
  ),

  # ── TAB 1: Garden ────────────────────────────────────────────────────────────
  nav_panel(
    title = "🌸 Garden",
    div(
      style = paste0(
        "background:#E1F5EE; border-radius:20px;",
        "padding:8px 18px; margin-bottom:14px;",
        "display:inline-block; font-size:13px;",
        "color:#0F6E56; font-family:'Nunito',sans-serif;"
      ),
      "\U0001f338 Hover or click any bee or flower to explore its ecological traits"
    ),
    div(style = "text-align:center; padding: 32px 0 16px 0;",
      tags$h1("Pollinia", style = "font-size:2.5rem; font-weight:800; color:#3D3428; margin-bottom:4px;"),
      tags$p("Trait-mediated bee–flower interaction explorer",
             style = "font-size:1.15rem; color:#7a6e60; margin-bottom:4px;"),
      tags$p("Built on 24 real pollination networks | 4,352 interactions",
             style = "font-size:0.85rem; color:#aaa; font-style:italic;")
    ),
    fluidRow(
      column(6,
        div(class = "card",
          tags$h4("🌿 Load Data", style = "font-weight:700; margin-bottom:16px;"),
          radioButtons("data_source", label = NULL,
            choices = c("Built-in networks (Safariland)" = "builtin",
                        "Upload CSV" = "upload"),
            selected = "builtin"
          ),
          conditionalPanel(
            condition = "input.data_source == 'upload'",
            fileInput("upload_csv", "Upload interaction CSV",
                      accept = ".csv", placeholder = "Choose file..."),
            tags$small("Columns: bee_species, flower_species, visit_count",
                       style = "color:#aaa; font-style:italic;")
          ),
          br(),
          actionButton("load_garden", "🌱 Load Garden",
                       class = "btn-sage", width = "100%")
        )
      ),
      column(6,
        div(class = "card",
          tags$h4("📊 Network Stats", style = "font-weight:700; margin-bottom:16px;"),
          uiOutput("metric_pills")
        )
      )
    ),
    fluidRow(
      column(12,
        div(class = "card", style = "padding:16px;",
          div(id = "canvasWrap", style = "position:relative; width:100%;",
            tags$canvas(id = "gardenCanvas", width = "900", height = "380",
                        style = "width:100%; height:380px; border-radius:12px; display:block;"),
            div(id = "gardenTooltip", style = paste0(
              "display:none; position:absolute; pointer-events:none;",
              "background:#fff; border:1.5px solid #E8E0D0; border-radius:12px;",
              "padding:10px 14px; box-shadow:0 4px 16px rgba(0,0,0,0.10);",
              "font-family:Nunito,sans-serif; font-size:0.82rem; color:#3D3428;",
              "min-width:160px; z-index:99; white-space:nowrap;"
            ))
          ),
          br(),
          fluidRow(
            column(6,
              sliderInput("bee_speed", "🐝 Bee speed",
                          min = 0.5, max = 3, value = 1, step = 0.5, width = "100%")
            ),
            column(6,
              div(style = "text-align:center; padding-top:28px;",
                uiOutput("pollen_counter")
              )
            )
          )
        )
      )
    ),
    tags$script(HTML('
      (function() {
        var canvas, ctx, tooltip;
        var bees = [], flowers = [], pollenDots = [], hearts = [], sparkles = [];
        var pollenCount = 0, speedMult = 1;
        var mouseX = -999, mouseY = -999;
        var hoveredFlower = -1, hoveredBee = -1;
        var clickedFlower = -1, clickedBee = -1;
        var dimTimer = 0;

        var flowerNames = ["Aristotelia chilensis","Alstroemeria aurea","Rosa eglanteria",
          "Berberis darwinii","Mutisia decurrens","Calceolaria uniflora",
          "Schinus patagonicus","Acaena splendens","Ribes magellanicum"];
        var beeNames = ["Policana albopilosa","Ruizantheda mutabilis","Corynura prothysteres",
          "Allograpta/Toxomerus","Syrphus octomaculatus","Platycheirus sp.",
          "Ichneumonidae sp.","Bombus terrestris","Apis mellifera"];

        Shiny.addCustomMessageHandler("setSpeed", function(v) { speedMult = v; });

        function rnd(a,b) { return a + Math.random()*(b-a); }

        var flowerDefs = [
          { type:"tulip", color:"#F4C0D1" }, { type:"tulip",  color:"#C8C0E8" },
          { type:"daisy", color:"#EF9F27" }, { type:"wild",   color:"#B8DCF0" },
          { type:"wild",  color:"#F4C0D1" }
        ];

        function drawTulip(cx, cy, color, sway) {
          ctx.save(); ctx.translate(cx, cy);
          ctx.strokeStyle = "#5a8a50"; ctx.lineWidth = 3;
          ctx.beginPath(); ctx.moveTo(sway, 0); ctx.quadraticCurveTo(sway*0.5, -40, 0, -70);
          ctx.stroke();
          ctx.translate(sway*0.3, -70);
          ctx.fillStyle = color;
          for (var i=0;i<5;i++) {
            ctx.save(); ctx.rotate((i/5)*Math.PI*2);
            ctx.beginPath(); ctx.ellipse(0,-12,7,16,0,0,Math.PI*2);
            ctx.fill(); ctx.restore();
          }
          ctx.restore();
        }
        function drawDaisy(cx, cy, sway) {
          ctx.save(); ctx.translate(cx, cy);
          ctx.strokeStyle = "#5a8a50"; ctx.lineWidth = 2.5;
          ctx.beginPath(); ctx.moveTo(sway,0); ctx.quadraticCurveTo(sway*0.5,-30,0,-55); ctx.stroke();
          ctx.translate(sway*0.2, -55);
          ctx.fillStyle = "#FEFBF0";
          for (var i=0;i<8;i++) {
            ctx.save(); ctx.rotate((i/8)*Math.PI*2);
            ctx.beginPath(); ctx.ellipse(0,-10,5,12,0,0,Math.PI*2); ctx.fill(); ctx.restore();
          }
          ctx.fillStyle = "#EF9F27";
          ctx.beginPath(); ctx.arc(0,0,8,0,Math.PI*2); ctx.fill();
          ctx.restore();
        }
        function drawWild(cx, cy, color, sway) {
          ctx.save(); ctx.translate(cx, cy);
          ctx.strokeStyle = "#6a9a60"; ctx.lineWidth = 1.5;
          ctx.beginPath(); ctx.moveTo(sway,0); ctx.lineTo(sway*0.3,-40); ctx.stroke();
          ctx.translate(sway*0.2, -40);
          ctx.fillStyle = color;
          for (var i=0;i<5;i++) {
            ctx.save(); ctx.rotate((i/5)*Math.PI*2);
            ctx.beginPath(); ctx.ellipse(0,-5,4,7,0,0,Math.PI*2); ctx.fill(); ctx.restore();
          }
          ctx.fillStyle = "#fff"; ctx.beginPath(); ctx.arc(0,0,3,0,Math.PI*2); ctx.fill();
          ctx.restore();
        }

        function drawBee(b, alpha) {
          ctx.save(); ctx.translate(b.x, b.y);
          ctx.globalAlpha = (alpha !== undefined) ? alpha : 1;
          var ang = Math.atan2(b.vy || 0, b.vx || 0.1);
          ctx.rotate(ang);
          ctx.globalAlpha *= 0.55;
          ctx.fillStyle = "#cceeff";
          ctx.beginPath(); ctx.ellipse(-2,-9+Math.sin(b.wingPhase)*3,9,5,-0.4,0,Math.PI*2); ctx.fill();
          ctx.beginPath(); ctx.ellipse(-2, 9-Math.sin(b.wingPhase)*3,9,5, 0.4,0,Math.PI*2); ctx.fill();
          ctx.globalAlpha = (alpha !== undefined) ? alpha : 1;
          ctx.fillStyle = "#EF9F27";
          ctx.beginPath(); ctx.ellipse(0,0,10,7,0,0,Math.PI*2); ctx.fill();
          ctx.fillStyle = "#3D3428";
          for (var s=0;s<3;s++) ctx.fillRect(-3+s*4-10,-4,2,8);
          ctx.fillStyle = "#fff"; ctx.beginPath(); ctx.arc(8,-2,2.5,0,Math.PI*2); ctx.fill();
          ctx.fillStyle = "#222"; ctx.beginPath(); ctx.arc(8.8,-2,1.2,0,Math.PI*2); ctx.fill();
          ctx.restore();
        }

        function showTooltip(html, px, py) {
          if (!tooltip) return;
          var wrap = document.getElementById("canvasWrap");
          var ww = wrap ? wrap.offsetWidth : 900;
          var wh = canvas ? canvas.offsetHeight : 380;
          tooltip.innerHTML = html;
          tooltip.style.display = "block";
          var tw = tooltip.offsetWidth || 170, th = tooltip.offsetHeight || 90;
          var lx = px + 14, ly = py - th/2;
          if (lx + tw > ww - 8) lx = px - tw - 14;
          if (ly < 4) ly = 4;
          if (ly + th > wh - 4) ly = wh - th - 4;
          tooltip.style.left = lx + "px";
          tooltip.style.top  = ly + "px";
        }
        function hideTooltip() {
          if (tooltip) tooltip.style.display = "none";
          hoveredFlower = -1; hoveredBee = -1;
        }

        function addSparkle(x, y) {
          for (var i=0;i<6;i++) {
            var ang = (i/6)*Math.PI*2;
            sparkles.push({ x:x, y:y, ang:ang, r:0, alpha:1 });
          }
        }

        function canvasPos(e) {
          var rect = canvas.getBoundingClientRect();
          var scaleX = canvas.width  / rect.width;
          var scaleY = canvas.height / rect.height;
          return {
            x: (e.clientX - rect.left) * scaleX,
            y: (e.clientY - rect.top)  * scaleY
          };
        }

        function init() {
          canvas  = document.getElementById("gardenCanvas");
          tooltip = document.getElementById("gardenTooltip");
          if (!canvas) { setTimeout(init, 300); return; }
          ctx = canvas.getContext("2d");
          canvas.width  = canvas.offsetWidth  || 900;
          canvas.height = canvas.offsetHeight || 380;
          var W = canvas.width, H = canvas.height, groundY = H*0.62;

          var xs = [0.12,0.22,0.35,0.43,0.50,0.62,0.72,0.82,0.90];
          for (var i=0; i<xs.length; i++) {
            var fd = flowerDefs[i % flowerDefs.length];
            flowers.push({
              x: xs[i]*W, y: groundY - rnd(5,20),
              type: fd.type, color: fd.color,
              phase: rnd(0,Math.PI*2), scale:1, scaleDur:0,
              name: flowerNames[i % flowerNames.length],
              tubeDepth: Math.round(rnd(4,22)),
              nectar: Math.round(rnd(2,12)),
              visitors: Math.round(rnd(3,12)),
              swaySpeed: 0.8, swayAmt: 5
            });
          }
          // 9th flower override — Ribes magellanicum
          flowers[8].name      = "Ribes magellanicum";
          flowers[8].color     = "#B8DCF0";
          flowers[8].tubeDepth = Math.round(rnd(4,14));
          flowers[8].nectar    = Math.round(rnd(2,8));
          flowers[8].visitors  = Math.round(rnd(2,7));
          flowers[8].swaySpeed = 0.6;
          flowers[8].swayAmt   = 3.5;
          flowers[8].scale     = 0.85 + Math.random()*0.3;

          for (var b=0; b<3; b++) {
            var ti = Math.floor(rnd(0, flowers.length));
            bees.push({
              x: rnd(50,W-50), y: rnd(groundY-120, groundY-40),
              vx:0, vy:0, tx: flowers[ti].x, ty: flowers[ti].y-50,
              targetIdx: ti, wingPhase: rnd(0,Math.PI*2),
              waitTimer:0, paused:false, trail:[],
              name: beeNames[b % beeNames.length],
              tongue: Math.round(rnd(5,18)),
              body:   Math.round(rnd(8,20)),
              sociality: Math.random()<0.4 ? "social" : "solitary",
              lecty: Math.random()<0.7 ? "polylectic" : "oligolectic",
              flowersVisited: Math.round(rnd(4,15)),
              matchScore: Math.round(rnd(40,95))
            });
          }

          // Mouse events
          canvas.addEventListener("mousemove", function(e) {
            var p = canvasPos(e);
            mouseX = p.x; mouseY = p.y;
            var found = false;
            // Check bees first
            for (var bi=0; bi<bees.length; bi++) {
              var bee = bees[bi];
              var d = Math.sqrt((p.x-bee.x)*(p.x-bee.x)+(p.y-bee.y)*(p.y-bee.y));
              if (d < 18) {
                if (hoveredBee !== bi) { hoveredBee = bi; hoveredFlower = -1; }
                var tfl = flowers[bee.targetIdx] || flowers[0];
                var html = "<b>" + bee.name + "</b><br>" +
                  "Tongue: " + bee.tongue + "mm &nbsp;|&nbsp; Body: " + bee.body + "mm<br>" +
                  "Sociality: " + bee.sociality + " &nbsp;|&nbsp; " + bee.lecty + "<br>" +
                  "<span style=\'color:#EF9F27;font-weight:700;\'>Match score: " + bee.matchScore + "%</span><br>" +
                  "<span style=\'color:#888;font-size:0.78rem;\'>Target: " + tfl.name + "</span>";
                showTooltip(html, p.x, p.y);
                found = true; break;
              }
            }
            if (!found) {
              // Check flowers
              for (var fi=0; fi<flowers.length; fi++) {
                var fl = flowers[fi];
                var flTipY = fl.y - (fl.type==="tulip"?70 : fl.type==="daisy"?55 : 40);
                var d2 = Math.sqrt((p.x-fl.x)*(p.x-fl.x)+(p.y-flTipY)*(p.y-flTipY));
                if (d2 < 20) {
                  if (hoveredFlower !== fi) {
                    hoveredFlower = fi; hoveredBee = -1;
                    addSparkle(fl.x, flTipY);
                  }
                  var html2 = "<b>" + fl.name + "</b><br>" +
                    "Tube depth: " + fl.tubeDepth + "mm<br>" +
                    "Nectar: " + fl.nectar + " µL<br>" +
                    "<span style=\'color:#8BAF7C;font-weight:700;\'>Bee visitors: " + fl.visitors + "</span>";
                  showTooltip(html2, p.x, p.y);
                  found = true; break;
                }
              }
            }
            if (!found) hideTooltip();
          });

          canvas.addEventListener("mouseleave", function() { hideTooltip(); mouseX=-999; mouseY=-999; });

          canvas.addEventListener("click", function(e) {
            var p = canvasPos(e);
            var hitBee = -1, hitFlower = -1;
            for (var bi=0; bi<bees.length; bi++) {
              var d = Math.sqrt((p.x-bees[bi].x)*(p.x-bees[bi].x)+(p.y-bees[bi].y)*(p.y-bees[bi].y));
              if (d < 18) { hitBee = bi; break; }
            }
            if (hitBee >= 0) {
              if (clickedBee === hitBee) { bees[hitBee].paused = false; clickedBee = -1; }
              else {
                if (clickedBee >= 0 && clickedBee < bees.length) bees[clickedBee].paused = false;
                clickedBee = hitBee; clickedFlower = -1;
                bees[hitBee].paused = true;
                bees[hitBee].trail = [{x:bees[hitBee].x, y:bees[hitBee].y}];
              }
              return;
            }
            for (var fi=0; fi<flowers.length; fi++) {
              var fl = flowers[fi];
              var flTipY = fl.y - (fl.type==="tulip"?70 : fl.type==="daisy"?55 : 40);
              var d2 = Math.sqrt((p.x-fl.x)*(p.x-fl.x)+(p.y-flTipY)*(p.y-flTipY));
              if (d2 < 20) { hitFlower = fi; break; }
            }
            if (hitFlower >= 0) {
              clickedFlower = (clickedFlower === hitFlower) ? -1 : hitFlower;
              clickedBee = -1; dimTimer = 120;
            } else {
              // Click elsewhere — unpause
              if (clickedBee >= 0 && clickedBee < bees.length) bees[clickedBee].paused = false;
              clickedBee = -1; clickedFlower = -1;
            }
          });

          requestAnimationFrame(loop);
        }

        function loop(ts) {
          if (!canvas) return;
          var W = canvas.width, H = canvas.height, groundY = H*0.62;
          var t = ts * 0.001;

          // Background
          var skyGrad = ctx.createLinearGradient(0,0,0,groundY);
          skyGrad.addColorStop(0,"#e8f4fd"); skyGrad.addColorStop(1,"#FEFBF0");
          ctx.fillStyle = skyGrad; ctx.fillRect(0,0,W,groundY);
          ctx.fillStyle = "#a8c898";
          ctx.beginPath(); ctx.moveTo(0,groundY);
          ctx.bezierCurveTo(W*0.25,groundY-60,W*0.5,groundY-40,W,groundY);
          ctx.lineTo(W,H); ctx.lineTo(0,H); ctx.closePath(); ctx.fill();
          ctx.fillStyle = "#8BAF7C";
          ctx.beginPath(); ctx.moveTo(0,groundY+10);
          ctx.bezierCurveTo(W*0.3,groundY-25,W*0.65,groundY-15,W,groundY+10);
          ctx.lineTo(W,H); ctx.lineTo(0,H); ctx.closePath(); ctx.fill();
          ctx.fillStyle = "#6a9a55";
          ctx.fillRect(0, groundY+8, W, H-groundY-8);

          // Pollen trails
          for (var p=pollenDots.length-1; p>=0; p--) {
            var pd=pollenDots[p]; pd.alpha-=0.012;
            if (pd.alpha<=0){pollenDots.splice(p,1);continue;}
            ctx.globalAlpha=pd.alpha; ctx.fillStyle="#EF9F27";
            ctx.beginPath(); ctx.arc(pd.x,pd.y,2.5,0,Math.PI*2); ctx.fill();
            ctx.globalAlpha=1;
          }

          // Hearts
          for (var h=hearts.length-1; h>=0; h--) {
            var ht=hearts[h]; ht.y-=1.2; ht.alpha-=0.018;
            if(ht.alpha<=0){hearts.splice(h,1);continue;}
            ctx.globalAlpha=ht.alpha; ctx.fillStyle="#F4C0D1";
            ctx.font="16px serif"; ctx.fillText("♥",ht.x,ht.y); ctx.globalAlpha=1;
          }

          // Sparkles
          for (var sp=sparkles.length-1; sp>=0; sp--) {
            var sk=sparkles[sp]; sk.r+=1.5; sk.alpha-=0.025;
            if(sk.alpha<=0){sparkles.splice(sp,1);continue;}
            ctx.globalAlpha=sk.alpha; ctx.fillStyle="#EF9F27";
            ctx.beginPath();
            ctx.arc(sk.x+Math.cos(sk.ang)*sk.r, sk.y+Math.sin(sk.ang)*sk.r, 2.5,0,Math.PI*2);
            ctx.fill(); ctx.globalAlpha=1;
          }

          // Bee target line (hovered bee)
          if (hoveredBee >= 0 && hoveredBee < bees.length) {
            var hb = bees[hoveredBee];
            var tfl = flowers[hb.targetIdx] || flowers[0];
            ctx.save(); ctx.setLineDash([4,4]);
            ctx.strokeStyle="#EF9F27"; ctx.lineWidth=1.5;
            ctx.beginPath(); ctx.moveTo(hb.x,hb.y); ctx.lineTo(tfl.x,tfl.y-50); ctx.stroke();
            ctx.setLineDash([]); ctx.restore();
          }

          // Clicked bee trail
          if (clickedBee >= 0 && clickedBee < bees.length) {
            var cb = bees[clickedBee];
            if (cb.trail.length > 1) {
              ctx.save(); ctx.setLineDash([3,5]);
              ctx.strokeStyle="#EF9F27"; ctx.lineWidth=1.5; ctx.globalAlpha=0.5;
              ctx.beginPath(); ctx.moveTo(cb.trail[0].x, cb.trail[0].y);
              for (var ti2=1; ti2<cb.trail.length; ti2++) ctx.lineTo(cb.trail[ti2].x, cb.trail[ti2].y);
              ctx.stroke(); ctx.setLineDash([]); ctx.globalAlpha=1; ctx.restore();
            }
          }

          // Flowers
          for (var fi=0; fi<flowers.length; fi++) {
            var fl=flowers[fi];
            var sway=Math.sin(t*(fl.swaySpeed||0.8)+fl.phase)*(fl.swayAmt||5);
            if(fl.scaleDur>0){fl.scale=1+0.15*Math.sin((1-fl.scaleDur/20)*Math.PI);fl.scaleDur--;}
            else fl.scale=1;
            ctx.save(); ctx.translate(fl.x,fl.y); ctx.scale(fl.scale,fl.scale); ctx.translate(-fl.x,-fl.y);
            if(fl.type==="tulip") drawTulip(fl.x,fl.y,fl.color,sway);
            else if(fl.type==="daisy") drawDaisy(fl.x,fl.y,sway);
            else drawWild(fl.x,fl.y,fl.color,sway);
            ctx.restore();
            // Highlight ring on clicked flower
            if (clickedFlower === fi) {
              var fy2 = fl.y-(fl.type==="tulip"?70:fl.type==="daisy"?55:40);
              ctx.save(); ctx.strokeStyle="#EF9F27"; ctx.lineWidth=2.5;
              ctx.globalAlpha=0.7+0.3*Math.sin(t*4);
              ctx.beginPath(); ctx.arc(fl.x,fy2,20,0,Math.PI*2); ctx.stroke();
              ctx.globalAlpha=1; ctx.restore();
            }
          }

          // Bees
          if (dimTimer > 0) dimTimer--;
          for (var bi=0; bi<bees.length; bi++) {
            var bee=bees[bi];
            bee.wingPhase += 0.45*speedMult;

            // Determine dim alpha
            var bAlpha = 1;
            if (dimTimer > 0 && clickedFlower >= 0) {
              var tgtMatch = (bee.targetIdx === clickedFlower);
              bAlpha = tgtMatch ? 1 : 0.4;
            }
            if (clickedBee >= 0 && clickedBee !== bi) bAlpha = Math.min(bAlpha, 0.4);

            if (!bee.paused) {
              if (bee.waitTimer > 0) {
                bee.waitTimer -= speedMult;
              } else {
                var dx=bee.tx-bee.x, dy=bee.ty-bee.y;
                var dist=Math.sqrt(dx*dx+dy*dy);
                if (dist < 12) {
                  var fi2=bee.targetIdx;
                  if (fi2 < flowers.length) {
                    flowers[fi2].scaleDur=20;
                    hearts.push({x:flowers[fi2].x,y:flowers[fi2].y-80,alpha:1});
                    pollenCount++; Shiny.setInputValue("pollen_count",pollenCount);
                  }
                  bee.waitTimer=40/speedMult;
                  var nextIdx=Math.floor(rnd(0,flowers.length));
                  bee.targetIdx=nextIdx;
                  bee.tx=flowers[nextIdx].x+rnd(-10,10);
                  bee.ty=flowers[nextIdx].y-60+rnd(-10,10);
                } else {
                  var speed=1.8*speedMult;
                  bee.vx=(dx/dist)*speed+rnd(-0.3,0.3);
                  bee.vy=(dy/dist)*speed+rnd(-0.3,0.3);
                  bee.x+=bee.vx; bee.y+=bee.vy;
                  if(Math.random()<0.25) pollenDots.push({x:bee.x,y:bee.y,alpha:0.7});
                }
              }
              if (clickedBee === bi) bee.trail.push({x:bee.x, y:bee.y});
            }

            // Glow ring for bee matching clicked flower
            if (clickedFlower >= 0 && bee.targetIdx === clickedFlower && dimTimer > 0) {
              ctx.save(); ctx.strokeStyle="#EF9F27"; ctx.lineWidth=3;
              ctx.shadowColor="#EF9F27"; ctx.shadowBlur=8;
              ctx.beginPath(); ctx.arc(bee.x,bee.y,14,0,Math.PI*2); ctx.stroke();
              ctx.restore();
            }
            // Glow ring for clicked bee
            if (clickedBee === bi) {
              ctx.save(); ctx.strokeStyle="#EF9F27"; ctx.lineWidth=2.5;
              ctx.shadowColor="#EF9F27"; ctx.shadowBlur=10;
              ctx.beginPath(); ctx.arc(bee.x,bee.y,16,0,Math.PI*2); ctx.stroke();
              ctx.restore();
            }

            drawBee(bee, bAlpha);
          }

          // Clicked bee expanded card on canvas
          if (clickedBee >= 0 && clickedBee < bees.length) {
            var cb2 = bees[clickedBee];
            var cx2 = Math.min(cb2.x+20, W-180), cy2 = Math.max(cb2.y-130, 8);
            ctx.save();
            ctx.fillStyle="rgba(255,255,255,0.96)";
            ctx.beginPath(); ctx.roundRect(cx2,cy2,165,120,10); ctx.fill();
            ctx.strokeStyle="#E8E0D0"; ctx.lineWidth=1.5;
            ctx.beginPath(); ctx.roundRect(cx2,cy2,165,120,10); ctx.stroke();
            ctx.fillStyle="#3D3428"; ctx.font="bold 11px Nunito,sans-serif";
            ctx.fillText(cb2.name.substring(0,20), cx2+8, cy2+18);
            ctx.font="10px Nunito,sans-serif"; ctx.fillStyle="#666";
            ctx.fillText("Tongue: "+cb2.tongue+"mm  Body: "+cb2.body+"mm", cx2+8, cy2+34);
            ctx.fillText(cb2.sociality+"  |  "+cb2.lecty, cx2+8, cy2+50);
            ctx.fillText("Flowers visited: "+cb2.flowersVisited, cx2+8, cy2+66);
            ctx.fillStyle="#888"; ctx.fillText("Match score:", cx2+8, cy2+84);
            ctx.fillStyle="#8BAF7C";
            ctx.fillRect(cx2+8, cy2+90, 149, 8);
            ctx.fillStyle="#EF9F27";
            ctx.fillRect(cx2+8, cy2+90, Math.round(149*(cb2.matchScore/100)), 8);
            ctx.fillStyle="#3D3428"; ctx.font="bold 9px Nunito,sans-serif";
            ctx.fillText(cb2.matchScore+"%", cx2+8+Math.round(149*(cb2.matchScore/100))+2, cy2+99);
            ctx.fillStyle="#aaa"; ctx.font="9px Nunito,sans-serif";
            ctx.fillText("Click bee again to unpause", cx2+8, cy2+115);
            ctx.restore();
          }

          // Clicked flower visiting bees card
          if (clickedFlower >= 0 && dimTimer > 0) {
            var cfl = flowers[clickedFlower];
            var cflTipY = cfl.y-(cfl.type==="tulip"?70:cfl.type==="daisy"?55:40);
            var vBees = [];
            for (var bi2=0; bi2<bees.length; bi2++) {
              if (bees[bi2].targetIdx === clickedFlower) vBees.push(bees[bi2].name);
            }
            var cardX = Math.min(cfl.x-80, W-200), cardY = cflTipY+28;
            if (cardY + 70 > H) cardY = cflTipY - 90;
            ctx.save();
            ctx.fillStyle="rgba(255,255,255,0.96)";
            ctx.beginPath(); ctx.roundRect(cardX,cardY,190,Math.max(54,vBees.length*18+36),10); ctx.fill();
            ctx.strokeStyle="#E8E0D0"; ctx.lineWidth=1.5;
            ctx.beginPath(); ctx.roundRect(cardX,cardY,190,Math.max(54,vBees.length*18+36),10); ctx.stroke();
            ctx.fillStyle="#3D3428"; ctx.font="bold 11px Nunito,sans-serif";
            ctx.fillText("Visiting bees", cardX+10, cardY+18);
            if (vBees.length === 0) {
              ctx.fillStyle="#aaa"; ctx.font="10px Nunito,sans-serif";
              ctx.fillText("None currently en route", cardX+10, cardY+36);
            } else {
              for (var vi=0; vi<Math.min(vBees.length,3); vi++) {
                ctx.fillStyle="#EF9F27";
                ctx.beginPath(); ctx.arc(cardX+16, cardY+32+vi*18, 3, 0, Math.PI*2); ctx.fill();
                ctx.fillStyle="#3D3428"; ctx.font="10px Nunito,sans-serif";
                ctx.fillText(vBees[vi].substring(0,22), cardX+24, cardY+36+vi*18);
              }
            }
            ctx.restore();
          }

          // Pollen counter
          ctx.fillStyle="rgba(255,255,255,0.85)";
          ctx.beginPath(); ctx.roundRect(W-155,10,145,34,8); ctx.fill();
          ctx.fillStyle="#3D3428"; ctx.font="bold 13px Nunito,sans-serif";
          ctx.fillText("🌼 Pollinations: "+pollenCount, W-148, 32);

          requestAnimationFrame(loop);
        }

        document.addEventListener("DOMContentLoaded", function() { setTimeout(init, 400); });
        window.addEventListener("resize", function() {
          if (canvas) { canvas.width = canvas.offsetWidth; }
        });
      })();
    '))
  ),

  # ── TAB 2: Network ───────────────────────────────────────────────────────────
  nav_panel(
    title = "🕸 Network",
    div(
      style = paste0(
        "background:#FAEEDA; border-radius:20px;",
        "padding:8px 18px; margin-bottom:14px;",
        "display:inline-block; font-size:13px;",
        "color:#633806; font-family:'Nunito',sans-serif;"
      ),
      "\U0001f578 Hover nodes to see species details \u2014 node size = degree, edge width = visits"
    ),
    fluidRow(
      column(8,
        div(class = "card",
          tags$h4("🕸 Interaction Network", style = "font-weight:700;"),
          withSpinner(plotlyOutput("network_plot", height = "520px"), color = "#8BAF7C")
        )
      ),
      column(4,
        div(class = "card",
          tags$h4("📐 Network Metrics", style = "font-weight:700; margin-bottom:12px;"),
          tableOutput("metrics_table"),
          hr(style = "border-color:#E8E0D0;"),
          tags$h5("🔑 Keystone Species", style = "font-weight:700; margin-bottom:10px;"),
          tags$p(tags$strong("Top bees:"), style = "margin-bottom:6px;"),
          uiOutput("keystone_bees"),
          br(),
          tags$p(tags$strong("Top flowers:"), style = "margin-bottom:6px;"),
          uiOutput("keystone_flowers")
        )
      )
    )
  ),

  # ── TAB 3: Predict ───────────────────────────────────────────────────────────
  nav_panel(
    title = "🔮 Predict",
    fluidRow(
      column(4,
        div(class = "card",
          div(
            style = paste0(
              "background:#EEEDFE; border-radius:20px;",
              "padding:8px 18px; margin-bottom:14px;",
              "display:inline-block; font-size:13px;",
              "color:#534AB7; font-family:'Nunito',sans-serif;"
            ),
            "\U0001f52e Enter traits below then click Predict Interaction"
          ),
          tags$div(class = "section-header bee-icon", "Bee Traits"),
          numericInput("tongue_mm", "Tongue length (mm)", value = 8, min = 2, max = 20),
          numericInput("body_mm",   "Body length (mm)",   value = 12, min = 5, max = 22),
          selectInput("sociality", "Sociality",
                      choices = c("social", "solitary"), selected = "solitary"),
          selectInput("lecty", "Lecty",
                      choices = c("polylectic", "oligolectic"), selected = "polylectic"),
          tags$div(class = "section-header flower-icon", "Flower Traits"),
          numericInput("tube_mm",    "Tube depth (mm)",    value = 10, min = 1, max = 30),
          numericInput("corolla_mm", "Corolla diam (mm)",  value = 20, min = 5, max = 50),
          numericInput("nectar_ul",  "Nectar vol (µl)",    value = 5,  min = 0, max = 20),
          selectInput("color", "Floral colour",
                      choices = c("UV_yellow","white","pink_purple","red","yellow"),
                      selected = "yellow"),
          selectInput("symmetry", "Symmetry",
                      choices = c("actinomorphic","zygomorphic"), selected = "actinomorphic"),
          br(),
          actionButton("predict_btn", "🔮 Predict Interaction",
                       class = "btn-amber", width = "100%")
        )
      ),
      column(8,
        div(class = "result-card",
          uiOutput("flower_display"),
          uiOutput("prediction_result")
        ),
        br(),
        div(class = "card",
          tags$h5("Model Performance", style = "font-weight:700;"),
          withSpinner(plotlyOutput("benchmark_plot", height = "280px"), color = "#8BAF7C")
        )
      )
    )
  ),

  # ── TAB 4: Extinction ────────────────────────────────────────────────────────
  nav_panel(
    title = "💀 Extinction",
    div(
      style = paste0(
        "background:#FCEBEB; border-radius:20px;",
        "padding:8px 18px; margin-bottom:14px;",
        "display:inline-block; font-size:13px;",
        "color:#791F1F; font-family:'Nunito',sans-serif;"
      ),
      "\U0001f480 Set removal % and order, then click Simulate Extinction"
    ),
    fluidRow(
      column(4,
        div(class = "card",
          tags$h4("⚙️ Simulation Settings", style = "font-weight:700;"),
          sliderInput("ext_pct", "Remove X% of bee species",
                      min = 0, max = 100, value = 20, step = 5, post = "%"),
          radioButtons(
            "ext_method",
            "Removal order",
            choices = c(
              "Random removal"                    = "random",
              "Most-connected first (worst case)" = "degree"
            ),
            selected = "random"
          ),
          br(),
          actionButton("simulate_btn", "💀 Simulate Extinction",
                       class = "btn-red", width = "100%"),
          br(), br(),
          uiOutput("robustness_badge")
        )
      ),
      column(8,
        div(class = "card",
          withSpinner(plotlyOutput("extinction_plot", height = "380px"), color = "#8BAF7C")
        ),
        div(class = "card",
          tags$h5("🌸 Flower Vulnerability", style = "font-weight:700;"),
          withSpinner(DTOutput("vuln_table"), color = "#8BAF7C")
        )
      )
    )
  ),

  # ── TAB 5: About ─────────────────────────────────────────────────────────────
  nav_panel(
    title = "📖 About",
    div(style = "max-width:800px; margin:0 auto; padding:24px 16px;",

      # ── Section 1: App header ──────────────────────────────────────────────
      div(class = "card",
        div(style = "text-align:center; padding:12px 0 20px 0;",
          tags$h2("🌸 Pollinia", style = "font-size:2rem; font-weight:800; color:#3D3428; margin-bottom:6px;"),
          tags$p(
            "An interactive explorer for trait-mediated bee\u2013flower interaction networks. ",
            "Built on 24 real pollination networks spanning 405 plant species and 1,818 pollinator species.",
            style = "color:#7a6e60; font-size:1rem; max-width:560px; margin:0 auto;"
          )
        ),
        hr(style = "border-color:#E8E0D0;"),
        tags$p(tags$strong("Researcher: "), "Zukhruf Aslam", style = "margin:0; font-size:0.95rem;")
      ),

      # ── Section 2: Primary citation ────────────────────────────────────────
      tags$h5("📄 Primary Citation", style = "font-weight:700; margin:20px 0 10px 0;"),
      div(style = paste0(
        "border-left:3px solid #8BAF7C; background:#FEFBF0;",
        "padding:12px 16px; border-radius:0 8px 8px 0;",
        "font-size:13px; color:#3D3428; line-height:1.7;"
      ),
        tags$p(style = "margin:0;",
          "Pichler, M., Boreux, V., Klein, A.-M., Schleuning, M., Hartig, F., & Carvalheiro, L. (2020). ",
          tags$span("Machine learning algorithms to infer trait-matching and predict species interactions in ecological networks. ", style = "font-style:italic;"),
          tags$span("Methods in Ecology and Evolution, ", style = "font-style:italic;"),
          "11(2), 281\u2013293. ",
          tags$a("https://doi.org/10.1111/2041-210X.13329",
                 href = "https://doi.org/10.1111/2041-210X.13329",
                 target = "_blank",
                 style = "color:#8BAF7C; word-break:break-all;")
        )
      ),

      # ── Section 3: Datasets ────────────────────────────────────────────────
      tags$h5("📦 Datasets", style = "font-weight:700; margin:20px 0 10px 0;"),

      # Dataset 1 — bipartite
      div(style = "background:#fff; border:1.5px solid #E8E0D0; border-radius:12px; padding:14px 16px; margin-bottom:10px;",
        div(style = "display:flex; justify-content:space-between; align-items:flex-start;",
          tags$strong("bipartite R Package (Dormann et al.)", style = "color:#3D3428; font-size:0.95rem;"),
          tags$a("Visit CRAN", href = "https://cran.r-project.org/web/packages/bipartite/",
                 target = "_blank",
                 style = "background:#8BAF7C;color:#fff;border-radius:999px;padding:3px 12px;font-size:12px;text-decoration:none;white-space:nowrap;margin-left:10px;")
        ),
        tags$p("23 built-in quantitative pollination networks from peer-reviewed field studies. Primary data source for Pollinia.",
               style = "color:#7A6E64; font-size:13px; margin:6px 0 8px 0;"),
        tags$p(style = "font-size:12px; color:#999; margin:0; font-style:italic;",
          "Dormann, C. F., Fr\u00fcnd, J., & Gruber, B. (2024). bipartite: Visualising bipartite networks and calculating some (ecological) indices [R package version 2.23]. CRAN."
        )
      ),

      # Dataset 2 — Safariland
      div(style = "background:#fff; border:1.5px solid #E8E0D0; border-radius:12px; padding:14px 16px; margin-bottom:10px;",
        div(style = "display:flex; justify-content:space-between; align-items:flex-start;",
          tags$strong("Safariland Network (V\u00e1zquez & Aizen, 2004)", style = "color:#3D3428; font-size:0.95rem;"),
          tags$a("View paper", href = "https://doi.org/10.1890/02-0439",
                 target = "_blank",
                 style = "background:#8BAF7C;color:#fff;border-radius:999px;padding:3px 12px;font-size:12px;text-decoration:none;white-space:nowrap;margin-left:10px;")
        ),
        tags$p("Quantitative plant-pollinator network from Patagonia, Argentina. 9 plant species \u00d7 27 pollinator species, 39 interactions. Included in the bipartite package.",
               style = "color:#7A6E64; font-size:13px; margin:6px 0 8px 0;"),
        tags$p(style = "font-size:12px; color:#999; margin:0; font-style:italic;",
          "V\u00e1zquez, D. P., & Aizen, M. A. (2004). Asymmetric specialization: A pervasive feature of plant-pollinator interactions. Ecology, 85(5), 1251\u20131257. https://doi.org/10.1890/02-0439"
        )
      ),

      # Dataset 3 — Demo CSV
      div(style = "background:#fff; border:1.5px solid #E8E0D0; border-radius:12px; padding:14px 16px; margin-bottom:10px;",
        div(style = "display:flex; justify-content:space-between; align-items:flex-start;",
          tags$strong("Demo Upload Dataset (Safariland CSV)", style = "color:#3D3428; font-size:0.95rem;"),
          downloadButton("download_demo", "Download demo CSV",
            style = "background:#EF9F27;color:white;border:none;border-radius:20px;padding:4px 14px;font-size:12px;cursor:pointer;white-space:nowrap;margin-left:10px;")
        ),
        tags$p("A pre-formatted CSV you can upload directly into the Garden tab to test Pollinia with your own data. Contains bee_species, flower_species, visit_count columns.",
               style = "color:#7A6E64; font-size:13px; margin:6px 0 6px 0;"),
        tags$p("\u2b07\ufe0f Download this CSV then drag it into the Upload panel on the Garden tab to explore the network.",
               style = "font-size:12px; color:#8BAF7C; margin:0; font-weight:600;")
      ),

      # Dataset 4 — Web of Life
      div(style = "background:#fff; border:1.5px solid #E8E0D0; border-radius:12px; padding:14px 16px; margin-bottom:10px;",
        div(style = "display:flex; justify-content:space-between; align-items:flex-start;",
          tags$strong("Web of Life Database (Bascompte Lab, UZH)", style = "color:#3D3428; font-size:0.95rem;"),
          tags$a("Visit site", href = "https://www.web-of-life.es",
                 target = "_blank",
                 style = "background:#8BAF7C;color:#fff;border-radius:999px;padding:3px 12px;font-size:12px;text-decoration:none;white-space:nowrap;margin-left:10px;")
        ),
        tags$p("Global ecological networks database with 100+ pollination networks. Free to download.",
               style = "color:#7A6E64; font-size:13px; margin:6px 0 8px 0;"),
        tags$p(style = "font-size:12px; color:#999; margin:0; font-style:italic;",
          "This work has used the Web of Life dataset (www.web-of-life.es) \u2014 Bascompte Lab, University of Zurich."
        )
      ),

      # ── Section 4: GitHub ──────────────────────────────────────────────────
      div(style = "text-align:center; padding:16px 0 4px 0;",
        tags$a(
          href = "https://github.com/Zukhruf1/pollinia",
          target = "_blank",
          style = paste0(
            "display:inline-block;",
            "background:#3D3428;",
            "color:white;",
            "padding:6px 18px;",
            "border-radius:20px;",
            "font-size:13px;",
            "text-decoration:none;",
            "font-family:'Nunito',sans-serif;",
            "margin-top:8px;"
          ),
          "\U0001f4bb View on GitHub"
        )
      )
    )
  )
)

# ── SERVER ─────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # Generate demo CSV on startup
  if (!file.exists("www/Safariland_upload_demo.csv")) {
    tryCatch({
      data("Safariland", package = "bipartite", envir = environment())
      df_demo <- as.data.frame(Safariland) %>%
        tibble::rownames_to_column("flower_species") %>%
        tidyr::pivot_longer(-flower_species,
                            names_to  = "bee_species",
                            values_to = "visit_count") %>%
        dplyr::filter(visit_count > 0)
      write.csv(df_demo, "www/Safariland_upload_demo.csv", row.names = FALSE)
    }, error = function(e) NULL)
  }

  # Download handler for demo CSV
  output$download_demo <- downloadHandler(
    filename = "Safariland_upload_demo.csv",
    content  = function(file) {
      file.copy("www/Safariland_upload_demo.csv", file)
    }
  )

  # Reactive data
  edges_rv <- reactiveVal(default_edges)

  # ── Garden: load data ────────────────────────────────────────────────────────
  observeEvent(input$load_garden, {
    if (input$data_source == "upload" && !is.null(input$upload_csv)) {
      df <- tryCatch(
        read_csv(input$upload_csv$datapath, show_col_types = FALSE),
        error = function(e) default_edges
      )
      if (all(c("flower_species","bee_species","visit_count") %in% names(df))) {
        edges_rv(df %>% filter(visit_count > 0))
      }
    } else {
      edges_rv(default_edges)
    }
  })

  # Bee speed -> JS
  observeEvent(input$bee_speed, {
    session$sendCustomMessage("setSpeed", input$bee_speed)
  })

  # Pollen counter
  output$pollen_counter <- renderUI({
    n <- input$pollen_count %||% 0
    tags$span(paste("🌼 Pollination events:", n),
              class = "metric-pill pill-amber",
              style = "font-size:1rem;")
  })

  # Metric pills
  output$metric_pills <- renderUI({
    df <- edges_rv()
    n_b <- n_distinct(df$bee_species)
    n_f <- n_distinct(df$flower_species)
    n_i <- nrow(df)
    conn <- round(net_metrics_vals["connectance"], 3)
    tagList(
      div(style = "display:flex; flex-wrap:wrap; gap:8px;",
        tags$span(paste("🐝 Bees:", n_b),       class = "metric-pill pill-amber"),
        tags$span(paste("🌸 Flowers:", n_f),     class = "metric-pill pill-sage"),
        tags$span(paste("🔗 Interactions:", n_i),class = "metric-pill pill-rose"),
        tags$span(paste("Connectance:", conn),   class = "metric-pill pill-lavender")
      )
    )
  })

  # ── Network tab ──────────────────────────────────────────────────────────────
  output$network_plot <- renderPlotly({

    `%||%` <- function(a, b) if (!is.null(a)) a else b

    ig  <- igraph::graph_from_incidence_matrix(web, weighted = TRUE)
    ly  <- igraph::layout_as_bipartite(ig)

    V(ig)$group <- ifelse(V(ig)$type, "Bee", "Flower")
    V(ig)$deg   <- igraph::degree(ig)
    V(ig)$px    <- ly[, 1]
    V(ig)$py    <- ly[, 2]
    V(ig)$col   <- ifelse(V(ig)$type, "#EF9F27", "#8BAF7C")
    V(ig)$sz    <- scales::rescale(V(ig)$deg, to = c(10, 35))
    V(ig)$lbl   <- sapply(V(ig)$name, function(n) {
      pts <- strsplit(n, " ")[[1]]
      if (length(pts) >= 2) paste0(substr(pts[1], 1, 1), ". ", pts[2]) else n
    })
    V(ig)$tip <- paste0(
      "<b>", V(ig)$name, "</b><br>",
      "Type: ",   V(ig)$group, "<br>",
      "Degree: ", V(ig)$deg
    )

    el  <- igraph::as_edgelist(ig)
    wts <- E(ig)$weight %||% rep(1, nrow(el))
    wn  <- scales::rescale(wts, to = c(0.4, 3.5))

    nd <- data.frame(
      name  = V(ig)$name,
      x     = V(ig)$px,
      y     = V(ig)$py,
      group = V(ig)$group,
      col   = V(ig)$col,
      sz    = V(ig)$sz,
      lbl   = V(ig)$lbl,
      tip   = V(ig)$tip,
      stringsAsFactors = FALSE
    )

    p <- plotly::plot_ly(source = "network")

    for (i in seq_len(nrow(el))) {
      s  <- el[i, 1]; tg <- el[i, 2]
      xs <- nd$x[nd$name == s]
      ys <- nd$y[nd$name == s]
      xe <- nd$x[nd$name == tg]
      ye <- nd$y[nd$name == tg]
      p  <- plotly::add_trace(
        p,
        type       = "scatter", mode = "lines",
        x          = c(xs, xe, NA),
        y          = c(ys, ye, NA),
        line       = list(color = "#F4C0D1", width = wn[i]),
        hoverinfo  = "none",
        showlegend = FALSE
      )
    }

    # Flower trace — markers + labels (9 nodes, readable)
    sub_f <- nd[nd$group == "Flower", ]
    p <- plotly::add_trace(
      p,
      type         = "scatter", mode = "markers+text",
      x            = sub_f$x,
      y            = sub_f$y,
      text         = sub_f$lbl,
      textposition = "top center",
      textfont     = list(size=9, color="#3D3428", family="Nunito, sans-serif"),
      marker       = list(size=sub_f$sz, color=sub_f$col,
                          line=list(color="white", width=1.5)),
      customdata   = sub_f$tip,
      hovertemplate = "%{customdata}<extra></extra>",
      name         = "Flower",
      showlegend   = TRUE
    )

    # Bee trace — markers only (27 nodes, hover for name)
    sub_b <- nd[nd$group == "Bee", ]
    p <- plotly::add_trace(
      p,
      type         = "scatter", mode = "markers",
      x            = sub_b$x,
      y            = sub_b$y,
      marker       = list(size=sub_b$sz, color=sub_b$col,
                          line=list(color="white", width=1.5)),
      customdata   = sub_b$tip,
      hovertemplate = "%{customdata}<extra></extra>",
      name         = "Bee",
      showlegend   = TRUE
    )

    p %>% plotly::layout(
      paper_bgcolor = "#FEFBF0",
      plot_bgcolor  = "#FEFBF0",
      xaxis  = list(visible=FALSE, showgrid=FALSE, zeroline=FALSE),
      yaxis  = list(visible=FALSE, showgrid=FALSE, zeroline=FALSE),
      legend = list(
        orientation = "h", y = -0.05,
        font = list(family="Nunito, sans-serif", color="#3D3428", size=12)
      ),
      hoverlabel = list(
        bgcolor="white", bordercolor="#E8E0D0",
        font=list(family="Nunito, sans-serif", color="#3D3428", size=12)
      ),
      margin = list(t=20, b=50, l=10, r=10)
    )
  })

  output$metrics_table <- renderTable({
    data.frame(
      Metric = c("Connectance","Nestedness","H2 (specialisation)","Modularity Q"),
      Value  = round(as.numeric(net_metrics_vals), 4)
    )
  }, striped = TRUE, bordered = FALSE, spacing = "s",
     rownames = FALSE, width = "100%")

  output$keystone_bees <- renderUI({
    if (is.null(sp_metrics_vals)) return(tags$em("No data"))
    top <- sp_metrics_vals$`higher level` %>%
      rownames_to_column("species") %>%
      arrange(desc(betweenness)) %>% head(3)
    tags$div(lapply(top$species, function(s) tags$span(s, class = "keystone-bee")))
  })

  output$keystone_flowers <- renderUI({
    if (is.null(sp_metrics_vals)) return(tags$em("No data"))
    top <- sp_metrics_vals$`lower level` %>%
      rownames_to_column("species") %>%
      arrange(desc(betweenness)) %>% head(3)
    tags$div(lapply(top$species, function(s) tags$span(s, class = "keystone-flower")))
  })

  # ── Predict tab ──────────────────────────────────────────────────────────────
  output$flower_display <- renderUI({
    tags$div(
      style = paste0(
        "text-align:center; padding:30px 20px;",
        "background:#FEFBF0; border-radius:14px;",
        "border: 1.5px dashed #E8E0D0;"
      ),
      tags$svg(
        xmlns="http://www.w3.org/2000/svg",
        viewBox="0 0 120 120", width="90", height="90",
        tags$line(x1="60",y1="115",x2="60",y2="78",
          stroke="#8BAF7C",`stroke-width`="4",
          `stroke-linecap`="round"),
        tags$ellipse(cx="60",cy="50",rx="11",ry="22",
          fill="#D3D1C7",opacity="0.6",
          transform="rotate(0 60 70)"),
        tags$ellipse(cx="60",cy="50",rx="11",ry="22",
          fill="#D3D1C7",opacity="0.6",
          transform="rotate(72 60 70)"),
        tags$ellipse(cx="60",cy="50",rx="11",ry="22",
          fill="#D3D1C7",opacity="0.6",
          transform="rotate(144 60 70)"),
        tags$ellipse(cx="60",cy="50",rx="11",ry="22",
          fill="#D3D1C7",opacity="0.6",
          transform="rotate(216 60 70)"),
        tags$ellipse(cx="60",cy="50",rx="11",ry="22",
          fill="#D3D1C7",opacity="0.6",
          transform="rotate(288 60 70)"),
        tags$circle(cx="60",cy="70",r="11",fill="#B4B2A9"),
        tags$circle(cx="60",cy="70",r="5",fill="#888780")
      ),
      tags$p(
        "Set bee and flower traits, then click",
        tags$br(),
        tags$strong("Predict Interaction",
          style="color:#8BAF7C;"),
        style=paste0(
          "color:#7A6E64; font-size:13px;",
          "margin:12px 0 0 0; line-height:1.6;"
        )
      )
    )
  })

  output$prediction_result <- renderUI({
    tags$div(
      style=paste0(
        "background:#FEFBF0; border-radius:14px;",
        "padding:16px 20px; text-align:center;",
        "border:1.5px dashed #E8E0D0; margin:8px 0;"
      ),
      tags$p(
        "Your prediction will appear here",
        style="color:#B4B2A9; font-size:13px; margin:0;"
      )
    )
  })

  observeEvent(input$predict_btn, {
    req(input$tongue_mm, input$body_mm, input$tube_mm,
        input$corolla_mm, input$nectar_ul)

    new_obs <- data.frame(
      tube_depth_mm     = as.numeric(input$tube_mm),
      corolla_diam_mm   = as.numeric(input$corolla_mm),
      nectar_vol_ul     = as.numeric(input$nectar_ul),
      nectar_sugar_pct  = 30,
      tongue_length_mm  = as.numeric(input$tongue_mm),
      body_length_mm    = as.numeric(input$body_mm),
      flight_range_m    = 500,
      flower_degree     = 5,
      flower_betweenness = 0.1,
      flower_closeness  = 0.3,
      bee_degree        = 5,
      bee_betweenness   = 0.1,
      bee_closeness     = 0.3,
      tongue_tube_ratio = as.numeric(input$tongue_mm) / max(as.numeric(input$tube_mm), 0.1),
      tongue_tube_match = abs(as.numeric(input$tongue_mm) - as.numeric(input$tube_mm)),
      body_flower_ratio = as.numeric(input$body_mm) / max(as.numeric(input$corolla_mm), 0.1),
      degree_product    = 25,
      centrality_sum    = 0.2,
      color_UV_yellow   = as.integer(input$color == "UV_yellow"),
      color_white       = as.integer(input$color == "white"),
      color_pink_purple = as.integer(input$color == "pink_purple"),
      color_red         = as.integer(input$color == "red"),
      color_yellow      = as.integer(input$color == "yellow"),
      sym_actino        = as.integer(input$symmetry == "actinomorphic"),
      soc_social        = as.integer(input$sociality == "social"),
      lec_polylectic    = as.integer(input$lecty == "polylectic")
    )

    cat("=== Predict debug ===\n")
    cat("Input color:", input$color, "\n")
    cat("Input tongue:", input$tongue_mm, "\n")
    cat("Input tube:", input$tube_mm, "\n")
    cat("new_obs columns:", paste(names(new_obs), collapse=", "), "\n")
    cat("tongue_tube_ratio:", new_obs$tongue_tube_ratio, "\n")
    cat("color_pink_purple:", new_obs$color_pink_purple, "\n")

    expected <- c(
      "tube_depth_mm","corolla_diam_mm","nectar_vol_ul",
      "nectar_sugar_pct","tongue_length_mm","body_length_mm",
      "flight_range_m","flower_degree","flower_betweenness",
      "flower_closeness","bee_degree","bee_betweenness",
      "bee_closeness","tongue_tube_ratio","tongue_tube_match",
      "body_flower_ratio","degree_product","centrality_sum",
      "color_UV_yellow","color_white","color_pink_purple",
      "color_red","color_yellow","sym_actino",
      "soc_social","lec_polylectic"
    )

    missing_cols <- setdiff(expected, names(new_obs))
    extra_cols   <- setdiff(names(new_obs), expected)

    if (length(missing_cols) > 0) {
      cat("MISSING columns:", paste(missing_cols, collapse=", "), "\n")
      for (col in missing_cols) new_obs[[col]] <- 0
    }
    if (length(extra_cols) > 0) {
      cat("EXTRA columns:", paste(extra_cols, collapse=", "), "\n")
      new_obs <- new_obs[, expected, drop=FALSE]
    }

    new_obs <- new_obs[, expected, drop=FALSE]
    cat("Final new_obs ncol:", ncol(new_obs), "\n")

    prob <- tryCatch({
      if (is.null(rf_model)) stop("rf_model is NULL")
      p <- predict(rf_model, newdata=new_obs, type="prob")
      cat("Raw prob:", p[1,"yes"], "\n")
      round(p[1,"yes"] * 100, 1)
    }, error=function(e){
      cat("Prediction error:", e$message, "\n")
      NA
    })

    cat("Final probability:", prob, "%\n")

    petal_colors <- c(
      UV_yellow   = "#EF9F27",
      white       = "#F5F0E8",
      pink_purple = "#C8C0E8",
      red         = "#E24B4A",
      yellow      = "#FAC775"
    )
    pc <- petal_colors[[input$color]]
    if (is.null(pc)) pc <- "#EF9F27"

    output$flower_display <- renderUI({
      tags$div(
        style="text-align:center;padding:16px 0;",
        tags$svg(
          xmlns="http://www.w3.org/2000/svg",
          viewBox="0 0 120 120", width="100", height="100",
          tags$line(x1="60",y1="115",x2="60",y2="78",
            stroke="#8BAF7C",`stroke-width`="4",`stroke-linecap`="round"),
          tags$ellipse(cx="60",cy="50",rx="11",ry="22",fill=pc,opacity="0.9",
            transform="rotate(0 60 70)"),
          tags$ellipse(cx="60",cy="50",rx="11",ry="22",fill=pc,opacity="0.9",
            transform="rotate(72 60 70)"),
          tags$ellipse(cx="60",cy="50",rx="11",ry="22",fill=pc,opacity="0.9",
            transform="rotate(144 60 70)"),
          tags$ellipse(cx="60",cy="50",rx="11",ry="22",fill=pc,opacity="0.9",
            transform="rotate(216 60 70)"),
          tags$ellipse(cx="60",cy="50",rx="11",ry="22",fill=pc,opacity="0.9",
            transform="rotate(288 60 70)"),
          tags$circle(cx="60",cy="70",r="11",fill="#EF9F27"),
          tags$circle(cx="60",cy="70",r="5", fill="#633806")
        )
      )
    })

    output$prediction_result <- renderUI({
      if (is.na(prob) || is.null(rf_model)) {
        tags$div(
          style=paste0("background:#FFF3E0;border-radius:14px;",
                       "padding:16px 20px;border:1.5px solid #E8E0D0;",
                       "text-align:center;margin:8px 0;"),
          tags$p("Model not loaded. Check console for errors.",
                 style="color:#E24B4A;font-size:13px;margin:0;font-weight:500;")
        )
      } else {
        is_likely <- prob > 50
        bg  <- if(is_likely) "#E8F5E9" else "#FFF3E0"
        bd  <- if(is_likely) "#8BAF7C" else "#EF9F27"
        tc  <- if(is_likely) "#27500A" else "#633806"
        lbl <- if(is_likely) "Likely Interaction 🌸" else "Unlikely Interaction 🌿"
        tags$div(
          style=paste0("background:",bg,";border-radius:14px;",
                       "padding:18px 22px;border:2px solid ",bd,";",
                       "text-align:center;margin:8px 0;"),
          tags$h3(lbl, style=paste0("color:",tc,";margin:0 0 8px 0;",
                                     "font-size:1.15rem;font-weight:700;")),
          tags$p(paste0("Interaction probability: ", prob, "%"),
                 style="color:#3D3428;font-size:15px;font-weight:600;margin:0 0 10px 0;"),
          tags$div(
            style="background:#E8E0D0;border-radius:20px;height:10px;overflow:hidden;",
            tags$div(style=paste0("width:",prob,"%;background:",bd,";",
                                  "height:100%;border-radius:20px;",
                                  "transition:width 0.6s ease;"))
          )
        )
      }
    })
  })

  output$benchmark_plot <- renderPlotly({
    if (is.null(benchmark_df)) {
      plotly::plotly_empty() %>%
        plotly::layout(
          title=list(text="Benchmark unavailable",
                     font=list(color="#7A6E64",size=13)),
          paper_bgcolor="#FEFBF0", plot_bgcolor="#FEFBF0")
    } else {
      df_long <- tidyr::pivot_longer(
        benchmark_df, -Model,
        names_to="Metric", values_to="Value"
      )
      plotly::plot_ly(
        df_long, x=~Metric, y=~Value, color=~Model, type="bar",
        colors=c("#EF9F27","#8BAF7C","#C8C0E8")
      ) %>%
        plotly::layout(
          barmode="group",
          yaxis=list(range=c(0,1), title="Score", gridcolor="#E8E0D0"),
          xaxis=list(title=""),
          paper_bgcolor="#FEFBF0",
          plot_bgcolor="#FEFBF0",
          legend=list(orientation="h", y=-0.25),
          font=list(family="Nunito, sans-serif", color="#3D3428"),
          margin=list(t=20,b=60)
        )
    }
  })

  # ── Extinction tab ────────────────────────────────────────────────────────────
  ext_result <- eventReactive(input$simulate_btn, {
    method <- input$ext_method
    tryCatch({
      set.seed(42)
      ext <- second.extinct(web, participant = "higher",
                            method = method, nrep = 30, details = FALSE)
      rob <- robustness(ext)
      # Build curve
      df_curve <- as.data.frame(ext)
      list(curve = df_curve, rob = rob, method = method)
    }, error = function(e) NULL)
  })

  output$extinction_plot <- renderPlotly({
    pct <- input$ext_pct   # reactive dependency on slider
    res <- ext_result()
    if (is.null(res)) {
      return(
        plotly::plotly_empty() %>%
          plotly::layout(
            title = list(
              text = "Set options above then click Simulate Extinction",
              font = list(color="8BAF7C", size=13, family="Nunito, sans-serif")
            ),
            paper_bgcolor = "#FEFBF0",
            plot_bgcolor  = "#FEFBF0",
            annotations = list(list(
              text = paste0(
                "The curve shows flower species surviving",
                " as bee species are removed"
              ),
              x=0.5, y=0.4, xref="paper", yref="paper",
              showarrow=FALSE,
              font=list(color="#B4B2A9", size=12, family="Nunito, sans-serif")
            ))
          )
      )
    }
    df  <- res$curve
    rob <- res$rob
    col <- if (res$method == "random") "#B8DCF0" else "#E24B4A"

    # Normalize columns: second.extinct returns go/lower columns
    xs <- seq(0, 100, length.out = nrow(df))
    ys <- if ("mean" %in% names(df)) df$mean else df[,1]
    ys_norm <- ys / max(ys, na.rm=TRUE) * 100

    cutoff_pct     <- pct
    cut_idx        <- which.min(abs(xs - cutoff_pct))
    survival_at_cut <- round(ys_norm[cut_idx], 1)

    plot_ly() %>%
      add_trace(x = xs, y = ys_norm, type = "scatter", mode = "lines",
                fill = "tozeroy", fillcolor = paste0(col,"44"),
                line = list(color = col, width = 2.5),
                name = res$method) %>%
      layout(
        paper_bgcolor = "#FEFBF0", plot_bgcolor = "#FEFBF0",
        font  = list(family = "Nunito"),
        xaxis = list(title = "Bee species removed (%)", range = c(0,100)),
        yaxis = list(title = "Flower species surviving (%)", range = c(0,105)),
        title = list(text = "Co-extinction cascade", font = list(size=14)),
        shapes = list(list(
          type = "line",
          x0   = cutoff_pct, x1 = cutoff_pct,
          y0   = 0,          y1 = 100,
          line = list(color="#EF9F27", width=2, dash="dot")
        )),
        annotations = list(
          list(
            x          = cutoff_pct,
            y          = survival_at_cut,
            text       = paste0(
              survival_at_cut, "% flowers survive<br>",
              "at ", cutoff_pct, "% bee loss"
            ),
            showarrow  = TRUE,
            arrowhead  = 2,
            arrowcolor = "#EF9F27",
            font       = list(color="#3D3428", size=11, family="Nunito, sans-serif"),
            bgcolor    = "white",
            bordercolor = "#E8E0D0",
            borderwidth = 1
          ),
          list(
            x=0.5, y=0.05, xref="paper", yref="paper",
            text       = paste0("Robustness: ", round(rob, 3)),
            showarrow  = FALSE,
            font       = list(color="#7A6E64", size=11, family="Nunito, sans-serif")
          )
        )
      )
  })

  output$robustness_badge <- renderUI({
    res <- ext_result()
    if (is.null(res)) return(NULL)
    tags$div(
      tags$span(paste("Robustness:", round(res$rob, 3)),
                class = "metric-pill pill-sage",
                style = "font-size:1rem;")
    )
  })

  output$vuln_table <- renderDT({
    df <- edges_rv()
    vuln <- df %>%
      group_by(flower_species) %>%
      summarise(
        n_bee_partners = n_distinct(bee_species),
        total_visits   = sum(visit_count),
        .groups        = "drop"
      ) %>%
      mutate(
        risk = case_when(
          n_bee_partners == 1 ~ "High",
          n_bee_partners <= 3 ~ "Medium",
          TRUE                ~ "Low"
        )
      ) %>%
      arrange(n_bee_partners)

    datatable(
      vuln,
      colnames = c("Flower species","Bee partners","Total visits","Risk"),
      options  = list(pageLength = 8, dom = "tp", scrollX = TRUE),
      rownames = FALSE,
      class    = "compact stripe"
    ) %>%
      formatStyle("risk",
        target           = "row",
        backgroundColor  = styleEqual(
          c("Low","Medium","High"),
          c("#8BAF7C22","#EF9F2722","#E24B4A22")
        )
      )
  })
}

# ── Launch ─────────────────────────────────────────────────────────────────────
shinyApp(ui, server)
