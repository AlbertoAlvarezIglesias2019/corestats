module.exports = {

    // =========================================================================
    // 1. INPUT MODE TOGGLE (Variables vs. Matrix Table)
    // =========================================================================
    input_mode_changed: function(ui, event) {
        module.exports.updateInputModeVisibility(ui);
    },

    updateInputModeVisibility: function(ui) {
        if (!ui.input_mode || !ui.variables_panel) return;

        const mode = ui.input_mode.value();
        const isMatrix = (mode === 'use_matrix');

        // Toggle Variables Panel
        if (isMatrix) {
            ui.variables_panel.$el.hide();
        } else {
            ui.variables_panel.$el.show();
        }

        // Toggle Table/Matrix input container (if present in current view)
        if (ui.table_input_container && ui.table_input_container.$el) {
            if (isMatrix) {
                ui.table_input_container.$el.show();
            } else {
                ui.table_input_container.$el.hide();
            }
        }

        // Toggle Summary input container (if present in current view)
        if (ui.summary_input_container && ui.summary_input_container.$el) {
            if (isMatrix) {
                ui.summary_input_container.$el.show();
            } else {
                ui.summary_input_container.$el.hide();
            }
        }
    },

    // =========================================================================
    // SHARED STYLES (Used by Size Sliders and Smooth Span)
    // =========================================================================
    getSharedSliderStyles: function() {
        return `
            <style>
                .slick-slider {
                    -webkit-appearance: none; appearance: none; flex-grow: 1; height: 6px;
                    background: #e2e8f0; border-radius: 9999px; outline: none; margin: 0; transition: background 0.2s ease;
                }
                .slick-slider:hover { background: #cbd5e1; }
                .slick-slider::-webkit-slider-thumb {
                    -webkit-appearance: none; appearance: none; width: 14px; height: 14px;
                    border-radius: 50%; background: #185adb; cursor: pointer;
                    box-shadow: 0 2px 5px rgba(24, 90, 219, 0.25); transition: transform 0.1s ease, background-color 0.1s ease;
                }
                .slick-slider::-webkit-slider-thumb:hover {
                    background: #0f44ab; transform: scale(1.2); box-shadow: 0 2px 7px rgba(24, 90, 219, 0.4);
                }
                .univ-panel { padding: 12px; border: 1px solid #dcdcdc; background-color: #f9f9f9; border-radius: 6px; font-family: sans-serif; margin-bottom: 10px; margin-top: 5px; width: 100%; box-sizing: border-box; }
                .univ-row { display: flex; align-items: center; background: white; padding: 6px 12px; border: 1px solid #ddd; border-radius: 4px; gap: 12px; box-sizing: border-box; margin-bottom: 6px; }
                .univ-row:last-child { margin-bottom: 0; }
                .univ-label { font-size: 12px; font-weight: bold; color: #444; width: 95px; flex-shrink: 0; }
                .univ-val { font-size: 12px; font-weight: bold; width: 35px; text-align: right; color: #333; }
            </style>
        `;
    },

    // =========================================================================
    // 2a. DYNAMIC SLIDERS (Sizes)
    // =========================================================================
    size_inputs_creating: function(ui, event) {
        if (!ui.size_inputs) return;
        let $element = ui.size_inputs.$el;

        let htmlContent = module.exports.getSharedSliderStyles() + `
            <div class="univ-panel">
                <div style="display: flex; flex-direction: column;">
        `;

        const sliders = [
            { name: 'font_size', id: 'font', label: 'Font Size', min: 8, max: 30, step: 1, def: 12, fix: 0 },
            { name: 'point_size', id: 'point', label: 'Point Size', min: 1, max: 15, step: 1, def: 2, fix: 1 },
            { name: 'line_size', id: 'line', label: 'Line Size', min: 0.2, max: 3.0, step: 0.1, def: 1.0, fix: 1 }
        ];

        sliders.forEach(s => {
            if (ui[s.name]) {
                let initVal = (ui[s.name].value() !== null) ? ui[s.name].value() : s.def;
                htmlContent += `
                    <div class="univ-row">
                        <span class="univ-label">${s.label}</span>
                        <input type="range" id="${s.id}-slider" name="${s.name}" class="slick-slider" min="${s.min}" max="${s.max}" step="${s.step}" value="${initVal}">
                        <span id="${s.id}-display" class="univ-val">${parseFloat(initVal).toFixed(s.fix)}</span>
                    </div>`;
            }
        });

        htmlContent += `</div></div>`;
        $element.html(htmlContent);

        // Bind events
        sliders.forEach(s => {
            if (ui[s.name]) {
                $element.find(`#${s.id}-slider`).on('input', function() {
                    const parsedVal = parseFloat(this.value);
                    $element.find(`#${s.id}-display`).text(parsedVal.toFixed(s.fix));
                    ui[s.name].setValue(parsedVal);
                });
            }
        });
    },

    // =========================================================================
    // 2b. DYNAMIC SLIDERS (Decimals) - Single Line Layout
    // =========================================================================
    decimal_inputs_creating: function(ui, event) {
        if (!ui.decimal_inputs) return;
        let $element = ui.decimal_inputs.$el;

        // Flexbox styles to put the title and sliders on one horizontal line
        let htmlContent = `
            <style>
                .dec-panel { 
                    display: flex; align-items: center; gap: 15px;
                    padding: 4px 8px; font-family: sans-serif; width: 100%; 
                    box-sizing: border-box; margin-bottom: 10px; 
                }
                .dec-main-label {
                    font-size: 12px; font-weight: bold; color: #444; flex-shrink: 0; margin-left: 8px;
                }
                .dec-row { 
                    display: flex; align-items: center; gap: 8px; flex-grow: 1;
                }
                .dec-label { font-size: 10px; font-weight: normal; color: #666; flex-shrink: 0; }
                .dec-val { font-size: 10px; color: #666; width: 15px; text-align: right; flex-shrink: 0; }
                
                /* Slider track - made slightly thicker and a darker gray, forced full opacity */
                .dec-slider { 
                    -webkit-appearance: none; appearance: none; flex-grow: 1; height: 4px; 
                    background: #cbd5e1 !important; border-radius: 9999px; outline: none; margin: 0; 
                    min-width: 40px; opacity: 1 !important; transition: background 0.2s ease;
                }
                .dec-slider:hover { background: #94a3b8 !important; }
                
                /* Slider thumb (the circle) - made slightly larger with a shadow */
                .dec-slider::-webkit-slider-thumb { 
                    -webkit-appearance: none; appearance: none; width: 12px; height: 12px; 
                    border-radius: 50%; background: #185adb !important; cursor: pointer; 
                    opacity: 1 !important; box-shadow: 0 1px 3px rgba(24, 90, 219, 0.4);
                    transition: transform 0.1s ease, background-color 0.1s ease;
                }
                .dec-slider::-webkit-slider-thumb:hover { 
                    background: #0f44ab !important; transform: scale(1.2); 
                }
            </style>
            <div class="dec-panel">
                <span class="dec-main-label">Decimals:</span>
        `;

        const sliders = [
            { name: 'nd_num', id: 'dec1', label: 'Numerical', min: 0, max: 5, step: 1, def: 1, fix: 0 },
            { name: 'nd_cat', id: 'dec2', label: 'Categorical', min: 0, max: 5, step: 1, def: 1, fix: 0 }
        ];

        sliders.forEach(s => {
            if (ui[s.name]) {
                let initVal = (ui[s.name].value() !== null) ? ui[s.name].value() : s.def;
                htmlContent += `
                    <div class="dec-row">
                        <span class="dec-label">${s.label}</span>
                        <input type="range" id="${s.id}-slider" name="${s.name}" class="dec-slider" min="${s.min}" max="${s.max}" step="${s.step}" value="${initVal}">
                        <span id="${s.id}-display" class="dec-val">${parseFloat(initVal).toFixed(s.fix)}</span>
                    </div>`;
            }
        });

        htmlContent += `</div>`;
        $element.html(htmlContent);

        // Bind events
        sliders.forEach(s => {
            if (ui[s.name]) {
                $element.find(`#${s.id}-slider`).on('input', function() {
                    const parsedVal = parseFloat(this.value);
                    $element.find(`#${s.id}-display`).text(parsedVal.toFixed(s.fix));
                    ui[s.name].setValue(parsedVal);
                });
            }
        });
    },

    // =========================================================================
    // 2c. DYNAMIC SLIDERS (Smooth Span)
    // =========================================================================
    smooth_container_creating: function(ui, event) {
        if (!ui.smooth_span) return;
        let $element = ui.smooth_container.$el;
        const initVal = (ui.smooth_span.value() !== null) ? ui.smooth_span.value() : 0.75;

        // Uses the shared styles so it matches seamlessly
        $element.html(module.exports.getSharedSliderStyles() + `
            <div class="univ-panel">
                <div class="univ-row" style="margin-bottom: 0;">
                    <span class="univ-label">Smooth Span</span>
                    <input type="range" id="smooth-slider" class="slick-slider" min="0.2" max="5.0" step="0.05" value="${initVal}">
                    <span id="smooth-display" class="univ-val">${parseFloat(initVal).toFixed(2)}</span>
                </div>
            </div>
        `);

        $element.find('#smooth-slider').on('input', function() {
            const parsedVal = parseFloat(this.value);
            $element.find('#smooth-display').text(parsedVal.toFixed(2));
            ui.smooth_span.setValue(parsedVal);
        });
    },

    // =========================================================================
    // 3. DYNAMIC PLOT/TABLE LABELS
    // =========================================================================
    plotLabels_creating: function(ui, event) {
        module.exports.renderLabelsPanel(ui);
    },

    renderLabelsPanel: function(ui) {
        if (!ui.plotLabels) return;
        let $container = ui.plotLabels.$el;
        $container.css({ 'width': '100%', 'max-width': 'none' });
        $container.parent().css({ 'width': '100%', 'max-width': 'none' });

        let htmlContent = `<div class="univ-panel"><div style="display: flex; flex-direction: column;">`;

        const textInputs = [
            { name: 'main_title', id: 't-main', label: 'Main Title', ph: 'Type main title...' },
            { name: 'main_subtitle', id: 't-sub', label: 'Subtitle', ph: 'Type subtitle...' },
            { name: 'x_axis_text', id: 't-xaxis', label: 'X-axis Label', ph: 'Type x-axis label...' },
            { name: 'y_axis_text', id: 't-yaxis', label: 'Y-axis Label', ph: 'Type y-axis label...' },
            { name: 'legend_title', id: 't-leg', label: 'Legend Title', ph: 'Type legend title...' },
            { name: 'panelby_text', id: 't-panel', label: 'Panel by Label', ph: 'Type panel by label...' },
            { name: 'groupby_text', id: 't-groupby', label: 'Group by Label', ph: 'Type group by label...' },
            { name: 'outcome_text', id: 't-outcome', label: 'Outcome Label', ph: 'Type outcome/s label...' },
            { name: 'margin_text', id: 't-margin', label: 'Marginals Label', ph: 'Type marginals label...' },
            { name: 'rows_text', id: 't-rows', label: 'Row Label', ph: 'Type rows label...' },
            { name: 'cols_text', id: 't-cols', label: 'Column Label', ph: 'Type columns label...' },
            { name: 'miss_text', id: 't-miss', label: 'Missing Label', ph: 'Type missing label...' }
        ];

        textInputs.forEach(t => {
            if (ui[t.name]) {
                let currentVal = ui[t.name].value() || "";
                htmlContent += `
                    <div class="univ-row">
                        <span class="univ-label">${t.label}</span>
                        <input type="text" id="${t.id}" value="${currentVal}" placeholder="${t.ph}" style="flex-grow: 1; padding: 4px 8px; border: 1px solid #ccc; border-radius: 4px; font-size: 12px; outline: none;">
                    </div>`;
            }
        });

        htmlContent += `</div></div>`;
        $container.html(htmlContent);

        // Bind events
        textInputs.forEach(t => {
            if (ui[t.name]) {
                $container.find(`#${t.id}`).on('input', function() { ui[t.name].setValue(this.value); });
            }
        });
    },

    // =========================================================================
    // 4a. MATRIX / CONTINGENCY TABLE INPUT
    // =========================================================================
    table_input_container_creating: function(ui, event) {
        module.exports.renderMatrixGrid(ui);
    },

    renderMatrixGrid: function(ui) {
        if (!ui.table_input_container) return;
        let $container = ui.table_input_container.$el;

        let $rowSelExisting = $container.find('#jmv-row-selector');
        let $colSelExisting = $container.find('#jmv-col-selector');

        let rows = $rowSelExisting.length ? parseInt($rowSelExisting.val()) : (ui.n_row ? (parseInt(ui.n_row.value()) || 2) : 2);
        let cols = $colSelExisting.length ? parseInt($colSelExisting.val()) : (ui.n_col ? (parseInt(ui.n_col.value()) || 2) : 2);

        let currentData = { row_labels: [], col_labels: [], matrix: [] };
        try {
            if (ui.table_json && ui.table_json.value()) {
                currentData = JSON.parse(ui.table_json.value());
                if (!$rowSelExisting.length) {
                    if (currentData.row_labels && currentData.row_labels.length) rows = currentData.row_labels.length;
                    if (currentData.col_labels && currentData.col_labels.length) cols = currentData.col_labels.length;
                }
            }
        } catch(e) {}

        if (ui.n_row) ui.n_row.setValue(rows);
        if (ui.n_col) ui.n_col.setValue(cols);

        let html = `
            <style>
                .mtx-panel { font-family: sans-serif; background: #f8fafc; border: 1px solid #cbd5e1; border-radius: 6px; padding: 14px; margin-top: 10px; width: 100%; box-sizing: border-box; }
                .mtx-title { font-size: 13px; font-weight: bold; color: #1e293b; margin-bottom: 12px; }
                .mtx-ctrl { display: flex; gap: 20px; align-items: center; margin-bottom: 14px; padding-bottom: 12px; border-bottom: 1px dashed #e2e8f0; }
                .mtx-lbl { font-size: 11px; font-weight: bold; color: #475569; text-transform: uppercase; margin-right: 6px;}
                .mtx-drop { padding: 4px 8px; font-size: 12px; border: 1px solid #cbd5e1; border-radius: 4px; outline: none; cursor: pointer; }
                .mtx-table { width: 100%; border-collapse: collapse; }
                .mtx-cell { border: 1px solid #e2e8f0; padding: 4px; background: white; text-align: center; }
                .mtx-head { background: #f1f5f9; font-weight: bold; font-size: 11px; color: #475569; }
                .mtx-input { width: 100%; border: none; outline: none; font-size: 12px; padding: 4px; box-sizing: border-box; text-align: center; background: transparent; }
                .mtx-btn-row { display: flex; justify-content: flex-end; margin-top: 14px; gap: 10px; }
                .btn-prim { background: #185adb; color: white; border: none; padding: 6px 16px; font-size: 11px; font-weight: bold; text-transform: uppercase; border-radius: 4px; cursor: pointer; }
                .btn-sec { background: white; color: #64748b; border: 1px solid #cbd5e1; padding: 6px 16px; font-size: 11px; font-weight: bold; text-transform: uppercase; border-radius: 4px; cursor: pointer; }
            </style>
            <div class="mtx-panel">
                <div class="mtx-title">Manual Data Input Settings</div>
                <div class="mtx-ctrl">
                    <div><span class="mtx-lbl">Rows</span><select id="jmv-row-selector" class="mtx-drop"></select></div>
                    <div><span class="mtx-lbl">Columns</span><select id="jmv-col-selector" class="mtx-drop"></select></div>
                </div>
                <table class="mtx-table">
                    <thead><tr><th class="mtx-cell mtx-head" style="width:100px;">Labels</th>
        `;

        for (let c = 0; c < cols; c++) {
            let colVal = (currentData.col_labels && currentData.col_labels[c]) || `Column ${c + 1}`;
            html += `<th class="mtx-cell mtx-head"><input type="text" class="mtx-input col-label-input" data-col="${c}" value="${colVal}" style="font-weight:bold;"></th>`;
        }
        html += `</tr></thead><tbody>`;

        for (let r = 0; r < rows; r++) {
            let rowVal = (currentData.row_labels && currentData.row_labels[r]) || `Row ${r + 1}`;
            html += `<tr><td class="mtx-cell" style="background:#f8fafc;"><input type="text" class="mtx-input row-label-input" data-row="${r}" value="${rowVal}" style="font-weight:bold;text-align:left;"></td>`;
            for (let c = 0; c < cols; c++) {
                let cellVal = (currentData.matrix && currentData.matrix[r] && currentData.matrix[r][c] !== undefined) ? currentData.matrix[r][c] : 0;
                html += `<td class="mtx-cell"><input type="number" class="mtx-input data-cell-input" data-row="${r}" data-col="${c}" value="${cellVal}"></td>`;
            }
            html += `</tr>`;
        }
        
        html += `</tbody></table>
            <div class="mtx-btn-row">
                <button id="jmv-btn-reset-table" class="btn-sec">Reset</button>
                <button id="jmv-btn-update-plot" class="btn-prim">Update Data</button>
            </div>
        </div>`;
        $container.html(html);

        let $rowSel = $container.find('#jmv-row-selector');
        let $colSel = $container.find('#jmv-col-selector');
        for (let i = 1; i <= 10; i++) {
            $rowSel.append(`<option value="${i}" ${i === rows ? 'selected' : ''}>${i}</option>`);
            $colSel.append(`<option value="${i}" ${i === cols ? 'selected' : ''}>${i}</option>`);
        }

        const triggerUpdate = function() {
            if (ui.n_row) ui.n_row.setValue(parseInt($rowSel.val()));
            if (ui.n_col) ui.n_col.setValue(parseInt($colSel.val()));
            module.exports.saveMatrixData(ui);
            module.exports.renderMatrixGrid(ui);
        };

        $rowSel.on('change', triggerUpdate);
        $colSel.on('change', triggerUpdate);
        $container.find('#jmv-btn-update-plot').on('click', () => module.exports.saveMatrixData(ui));
        $container.find('#jmv-btn-reset-table').on('click', function() {
            if (ui.n_row) ui.n_row.setValue(2);
            if (ui.n_col) ui.n_col.setValue(2);
            if (ui.table_json) ui.table_json.setValue('');
            setTimeout(() => { module.exports.renderMatrixGrid(ui); }, 50);
        });

        module.exports.saveMatrixData(ui);
    },

    saveMatrixData: function(ui) {
        if (!ui.table_input_container || !ui.table_json) return;
        let $container = ui.table_input_container.$el;
        let rows = parseInt($container.find('#jmv-row-selector').val());
        let cols = parseInt($container.find('#jmv-col-selector').val());
        let tableData = { row_labels: [], col_labels: [], matrix: [] };

        for (let r = 0; r < rows; r++) tableData.row_labels.push($container.find(`.row-label-input[data-row="${r}"]`).val() || `Row ${r + 1}`);
        for (let c = 0; c < cols; c++) tableData.col_labels.push($container.find(`.col-label-input[data-col="${c}"]`).val() || `Column ${c + 1}`);

        for (let r = 0; r < rows; r++) {
            let rowArray = [];
            for (let c = 0; c < cols; c++) {
                let val = parseFloat($container.find(`.data-cell-input[data-row="${r}"][data-col="${c}"]`).val());
                rowArray.push(isNaN(val) ? 0 : val);
            }
            tableData.matrix.push(rowArray);
        }
        ui.table_json.setValue(JSON.stringify(tableData));
    },

    // =========================================================================
    // 4b. MANUAL SUMMARY DATA INPUT (N, Mean, SD)
    // =========================================================================
    summary_input_container_creating: function(ui, event) {
        module.exports.renderSummaryInput(ui);
    },

    renderSummaryInput: function(ui) {
        if (!ui.summary_input_container) return;
        let $container = ui.summary_input_container.$el;

        let nVal = (ui.summ_n && ui.summ_n.value() !== null) ? ui.summ_n.value() : 100;
        let meanVal = (ui.summ_mean && ui.summ_mean.value() !== null) ? ui.summ_mean.value() : 50;
        let sdVal = (ui.summ_sd && ui.summ_sd.value() !== null) ? ui.summ_sd.value() : 20;

        let htmlContent = `
            <style>
                .sum-panel { 
                    font-family: sans-serif; 
                    background: #f8fafc; 
                    border: 1px solid #cbd5e1; 
                    border-radius: 6px; 
                    padding: 10px 12px; 
                    margin-top: 10px; 
                    width: fit-content; 
                    max-width: 210px; 
                    box-sizing: border-box; 
                }
                .sum-title { 
                    font-size: 12px; 
                    font-weight: bold; 
                    color: #1e293b; 
                    margin-bottom: 8px; 
                    white-space: nowrap; 
                }
                .sum-row { 
                    display: flex; 
                    align-items: center; 
                    justify-content: flex-start; 
                    background: white; 
                    padding: 4px 8px; 
                    border: 1px solid #e2e8f0; 
                    border-radius: 4px; 
                    gap: 8px; 
                    box-sizing: border-box; 
                    margin-bottom: 5px; 
                }
                .sum-row:last-child { margin-bottom: 0; }
                .sum-label { 
                    font-size: 11px; 
                    font-weight: bold; 
                    color: #475569; 
                    width: 38px; 
                    text-align: right; 
                    flex-shrink: 0; 
                }
                .sum-input { 
                    width: 65px; 
                    flex-grow: 0; 
                    padding: 3px 6px; 
                    border: 1px solid #cbd5e1; 
                    border-radius: 4px; 
                    font-size: 12px; 
                    outline: none; 
                    text-align: center; 
                }
            </style>
            <div class="sum-panel">
                <div class="sum-title">Summary Statistics Input</div>
                <div class="sum-row">
                    <span class="sum-label">N</span>
                    <input type="number" id="jmv-summ-n" class="sum-input" min="1" step="1" value="${nVal}">
                </div>
                <div class="sum-row">
                    <span class="sum-label">Mean</span>
                    <input type="number" id="jmv-summ-mean" class="sum-input" step="any" value="${meanVal}">
                </div>
                <div class="sum-row">
                    <span class="sum-label">SD</span>
                    <input type="number" id="jmv-summ-sd" class="sum-input" min="0" step="any" value="${sdVal}">
                </div>
            </div>
        `;

        $container.html(htmlContent);

        // Bind events to update Jamovi options
        $container.find('#jmv-summ-n').on('input', function() {
            if (ui.summ_n) ui.summ_n.setValue(parseInt(this.value) || 0);
        });
        $container.find('#jmv-summ-mean').on('input', function() {
            if (ui.summ_mean) ui.summ_mean.setValue(parseFloat(this.value) || 0);
        });
        $container.find('#jmv-summ-sd').on('input', function() {
            if (ui.summ_sd) ui.summ_sd.setValue(parseFloat(this.value) || 0);
        });
    },
    
    // =========================================================================
    // 5. LIFECYCLE MEMORY SYNC & RESTORE
    // =========================================================================
    view_loaded: function(ui, event) {
        module.exports.updateInputModeVisibility(ui);

        // Safely trigger all renders if their containers exist in the current UI
        if (ui.size_inputs && ui.size_inputs.$el) module.exports.size_inputs_creating(ui, event);
        if (ui.decimal_inputs && ui.decimal_inputs.$el) module.exports.decimal_inputs_creating(ui, event);
        if (ui.smooth_container && ui.smooth_container.$el) module.exports.smooth_container_creating(ui, event);
        if (ui.plotLabels && ui.plotLabels.$el) module.exports.renderLabelsPanel(ui);
        if (ui.table_input_container && ui.table_input_container.$el) module.exports.renderMatrixGrid(ui);
        if (ui.summary_input_container && ui.summary_input_container.$el) module.exports.renderSummaryInput(ui);
    }
};